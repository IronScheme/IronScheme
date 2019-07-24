using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Compiler
{
	// ==============================================================
	// ============          Encoding Guesser           =============
	// ==============================================================

	/// <summary>
	/// This class provides a simple finite state automaton that
	/// scans the file looking for (1) valid UTF-8 byte patterns,
	/// (2) bytes >= 0x80 which are not part of a UTF-8 sequence.
	/// The method then guesses whether it is UTF-8 or maybe some 
	/// local machine default encoding.  This works well for the
	/// various Latin encodings.
	/// </summary>
	internal class Guesser
	{
		ScanBuff buffer;

		public int GuessCodePage() { return Scan(); }

		const int maxAccept = 10;
		const int initial = 0;
		const int eofNum = 0;
		const int goStart = -1;
		const int INITIAL = 0;
		const int EndToken = 0;

		#region user code
		/* 
         *  Reads the bytes of a file to determine if it is 
         *  UTF-8 or a single-byte code page file.
         */
		public long utfX;
		public long uppr;
		#endregion user code

		int state;
		int currentStart = startState[0];
		int code;

		#region ScannerTables
		static int[] startState = new int[] { 11, 0 };

		#region CharacterMap
		static sbyte[] map = new sbyte[256] {
/*     '\0' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*   '\x10' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*   '\x20' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*      '0' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*      '@' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*      'P' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*      '`' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*      'p' */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
/*   '\x80' */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
/*   '\x90' */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
/*   '\xA0' */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
/*   '\xB0' */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
/*   '\xC0' */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
/*   '\xD0' */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
/*   '\xE0' */ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
/*   '\xF0' */ 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5 };
		#endregion

		static sbyte[][] nextState = new sbyte[][] {
			new sbyte[] {0, 0, 0, 0, 0, 0},
			new sbyte[] {-1, -1, 10, -1, -1, -1},
			new sbyte[] {-1, -1, -1, -1, -1, -1},
			new sbyte[] {-1, -1, 8, -1, -1, -1},
			new sbyte[] {-1, -1, 5, -1, -1, -1},
			new sbyte[] {-1, -1, 6, -1, -1, -1},
			new sbyte[] {-1, -1, 7, -1, -1, -1},
			null,
			new sbyte[] {-1, -1, 9, -1, -1, -1},
			null,
			null,
			new sbyte[] {-1, 1, 2, 3, 4, 2}
		};


		// Reason for suppression: cannot have self-reference in array initializer.
		static Guesser()
		{
			nextState[7] = nextState[2];
			nextState[9] = nextState[2];
			nextState[10] = nextState[2];
		}

		int NextState()
		{
			if (code == -1)
				return eofNum;
			else
				return nextState[state][map[code]];
		}
		#endregion

		public Guesser(System.IO.Stream file) { SetSource(file); }

		public void SetSource(System.IO.Stream source)
		{
			this.buffer = new Scanner.StreamBuff(source);
			code = buffer.Read();
		}

		int Scan()
		{
			for (; ; )
			{
				int next;
				state = currentStart;
				while ((next = NextState()) == goStart)
					code = buffer.Read();

				state = next;
				code = buffer.Read();

				while ((next = NextState()) > eofNum)
				{
					state = next;
					code = buffer.Read();
				}
				if (state <= maxAccept)
				{
					#region ActionSwitch
#pragma warning disable 162
					switch (state)
					{
						case eofNum:
							switch (currentStart)
							{
								case 11:
									if (utfX == 0 && uppr == 0) return -1; /* raw ascii */
									else if (uppr * 10 > utfX) return 0;   /* default code page */
									else return 65001;                     /* UTF-8 encoding */
									break;
							}
							return EndToken;
						case 1: // Recognized '{Upper128}',	Shortest string "\xC0"
						case 2: // Recognized '{Upper128}',	Shortest string "\x80"
						case 3: // Recognized '{Upper128}',	Shortest string "\xE0"
						case 4: // Recognized '{Upper128}',	Shortest string "\xF0"
							uppr++;
							break;
						case 5: // Recognized '{Utf8pfx4}{Utf8cont}',	Shortest string "\xF0\x80"
							uppr += 2;
							break;
						case 6: // Recognized '{Utf8pfx4}{Utf8cont}{2}',	Shortest string "\xF0\x80\x80"
							uppr += 3;
							break;
						case 7: // Recognized '{Utf8pfx4}{Utf8cont}{3}',	Shortest string "\xF0\x80\x80\x80"
							utfX += 3;
							break;
						case 8: // Recognized '{Utf8pfx3}{Utf8cont}',	Shortest string "\xE0\x80"
							uppr += 2;
							break;
						case 9: // Recognized '{Utf8pfx3}{Utf8cont}{2}',	Shortest string "\xE0\x80\x80"
							utfX += 2;
							break;
						case 10: // Recognized '{Utf8pfx2}{Utf8cont}',	Shortest string "\xC0\x80"
							utfX++;
							break;
						default:
							break;
					}
#pragma warning restore 162
					#endregion
				}
			}
		}
	} // end class Guesser
}

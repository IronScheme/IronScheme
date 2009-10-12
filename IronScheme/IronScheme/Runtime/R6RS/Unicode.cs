#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

namespace IronScheme.Runtime.R6RS
{
  public class Unicode : Builtins
  {
    [Builtin("string-downcase")]
    public static object ToLowerCaseString(object obj)
    {
      string s = RequiresNotNull<string>(obj);
      StringBuilder sb = new StringBuilder(s.ToLower());
      for (int i = 0; i < sb.Length; i++)
      {
        if (sb[i] == '\x03C3')
        {
          if (i == sb.Length - 1)
          {
            if (i != 0 && !char.IsSeparator(sb[i - 1]))
            {
              sb[i] = '\x03C2';
            }
          }
          else if (char.IsSeparator(sb[i + 1]))
          {
            sb[i] = '\x03C2';
          }
        }
      }
      return sb.ToString();
    }

    class CaseMap
    {
      char key;

      public char Key
      {
        get
        {
          if (key == 0 && Lower.Length == 1)
          {
            return Lower[0];
          }
          else
          {
            return key;
          }
        }
        set
        {
          key = value;
        }
      }

      public char[] Lower, Title, Upper;

      public static readonly CaseMap Partial = new CaseMap();

      public override string ToString()
      {
        if (this == Partial)
        {
          return "Partial";
        }
        else
        {
          return string.Format("Key: {3} Lower: {0} Title: {1} Upper: {2}", 
            new string(Lower), new string(Title), new string(Upper), Key);
        }
      }
    }

    #region Case map data

    static char[] MakeString(params char[] values)
    {
      return values;
    }

    readonly static CaseMap[] mappings =
    {
      new CaseMap { Lower = MakeString('\u00DF'), Title = MakeString('\u0053', '\u0073'), Upper = MakeString('\u0053', '\u0053') },
      new CaseMap { Key = '\u0130', Lower = MakeString('\u0069','\u0307'), Title = MakeString('\u0130'), Upper = MakeString('\u0130') }, 
      new CaseMap { Lower = MakeString('\uFB00'), Title = MakeString('\u0046','\u0066'), Upper = MakeString('\u0046','\u0046') }, 
      new CaseMap { Lower = MakeString('\uFB01'), Title = MakeString('\u0046','\u0069'), Upper = MakeString('\u0046','\u0049') }, 
      new CaseMap { Lower = MakeString('\uFB02'), Title = MakeString('\u0046','\u006C'), Upper = MakeString('\u0046','\u004C') }, 
      new CaseMap { Lower = MakeString('\uFB03'), Title = MakeString('\u0046','\u0066','\u0069'), Upper = MakeString('\u0046','\u0046','\u0049') }, 
      new CaseMap { Lower = MakeString('\uFB04'), Title = MakeString('\u0046','\u0066','\u006C'), Upper = MakeString('\u0046','\u0046','\u004C') }, 
      new CaseMap { Lower = MakeString('\uFB05'), Title = MakeString('\u0053','\u0074'), Upper = MakeString('\u0053','\u0054') }, 
      new CaseMap { Lower = MakeString('\uFB06'), Title = MakeString('\u0053','\u0074'), Upper = MakeString('\u0053','\u0054') }, 
      new CaseMap { Lower = MakeString('\u0587'), Title = MakeString('\u0535','\u0582'), Upper = MakeString('\u0535','\u0552') }, 
      new CaseMap { Lower = MakeString('\uFB13'), Title = MakeString('\u0544','\u0576'), Upper = MakeString('\u0544','\u0546') }, 
      new CaseMap { Lower = MakeString('\uFB14'), Title = MakeString('\u0544','\u0565'), Upper = MakeString('\u0544','\u0535') }, 
      new CaseMap { Lower = MakeString('\uFB15'), Title = MakeString('\u0544','\u056B'), Upper = MakeString('\u0544','\u053B') }, 
      new CaseMap { Lower = MakeString('\uFB16'), Title = MakeString('\u054E','\u0576'), Upper = MakeString('\u054E','\u0546') }, 
      new CaseMap { Lower = MakeString('\uFB17'), Title = MakeString('\u0544','\u056D'), Upper = MakeString('\u0544','\u053D') }, 
      new CaseMap { Lower = MakeString('\u0149'), Title = MakeString('\u02BC','\u004E'), Upper = MakeString('\u02BC','\u004E') }, 
      new CaseMap { Lower = MakeString('\u0390'), Title = MakeString('\u0399','\u0308','\u0301'), Upper = MakeString('\u0399','\u0308','\u0301') }, 
      new CaseMap { Lower = MakeString('\u03B0'), Title = MakeString('\u03A5','\u0308','\u0301'), Upper = MakeString('\u03A5','\u0308','\u0301') }, 
      new CaseMap { Lower = MakeString('\u01F0'), Title = MakeString('\u004A','\u030C'), Upper = MakeString('\u004A','\u030C') }, 
      new CaseMap { Lower = MakeString('\u1E96'), Title = MakeString('\u0048','\u0331'), Upper = MakeString('\u0048','\u0331') }, 
      new CaseMap { Lower = MakeString('\u1E97'), Title = MakeString('\u0054','\u0308'), Upper = MakeString('\u0054','\u0308') }, 
      new CaseMap { Lower = MakeString('\u1E98'), Title = MakeString('\u0057','\u030A'), Upper = MakeString('\u0057','\u030A') }, 
      new CaseMap { Lower = MakeString('\u1E99'), Title = MakeString('\u0059','\u030A'), Upper = MakeString('\u0059','\u030A') }, 
      new CaseMap { Lower = MakeString('\u1E9A'), Title = MakeString('\u0041','\u02BE'), Upper = MakeString('\u0041','\u02BE') }, 
      new CaseMap { Lower = MakeString('\u1F50'), Title = MakeString('\u03A5','\u0313'), Upper = MakeString('\u03A5','\u0313') }, 
      new CaseMap { Lower = MakeString('\u1F52'), Title = MakeString('\u03A5','\u0313','\u0300'), Upper = MakeString('\u03A5','\u0313','\u0300') }, 
      new CaseMap { Lower = MakeString('\u1F54'), Title = MakeString('\u03A5','\u0313','\u0301'), Upper = MakeString('\u03A5','\u0313','\u0301') }, 
      new CaseMap { Lower = MakeString('\u1F56'), Title = MakeString('\u03A5','\u0313','\u0342'), Upper = MakeString('\u03A5','\u0313','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FB6'), Title = MakeString('\u0391','\u0342'), Upper = MakeString('\u0391','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FC6'), Title = MakeString('\u0397','\u0342'), Upper = MakeString('\u0397','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FD2'), Title = MakeString('\u0399','\u0308','\u0300'), Upper = MakeString('\u0399','\u0308','\u0300') }, 
      new CaseMap { Lower = MakeString('\u1FD3'), Title = MakeString('\u0399','\u0308','\u0301'), Upper = MakeString('\u0399','\u0308','\u0301') }, 
      new CaseMap { Lower = MakeString('\u1FD6'), Title = MakeString('\u0399','\u0342'), Upper = MakeString('\u0399','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FD7'), Title = MakeString('\u0399','\u0308','\u0342'), Upper = MakeString('\u0399','\u0308','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FE2'), Title = MakeString('\u03A5','\u0308','\u0300'), Upper = MakeString('\u03A5','\u0308','\u0300') }, 
      new CaseMap { Lower = MakeString('\u1FE3'), Title = MakeString('\u03A5','\u0308','\u0301'), Upper = MakeString('\u03A5','\u0308','\u0301') }, 
      new CaseMap { Lower = MakeString('\u1FE4'), Title = MakeString('\u03A1','\u0313'), Upper = MakeString('\u03A1','\u0313') }, 
      new CaseMap { Lower = MakeString('\u1FE6'), Title = MakeString('\u03A5','\u0342'), Upper = MakeString('\u03A5','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FE7'), Title = MakeString('\u03A5','\u0308','\u0342'), Upper = MakeString('\u03A5','\u0308','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1FF6'), Title = MakeString('\u03A9','\u0342'), Upper = MakeString('\u03A9','\u0342') }, 
      new CaseMap { Lower = MakeString('\u1F80'), Title = MakeString('\u1F88'), Upper = MakeString('\u1F08','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F81'), Title = MakeString('\u1F89'), Upper = MakeString('\u1F09','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F82'), Title = MakeString('\u1F8A'), Upper = MakeString('\u1F0A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F83'), Title = MakeString('\u1F8B'), Upper = MakeString('\u1F0B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F84'), Title = MakeString('\u1F8C'), Upper = MakeString('\u1F0C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F85'), Title = MakeString('\u1F8D'), Upper = MakeString('\u1F0D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F86'), Title = MakeString('\u1F8E'), Upper = MakeString('\u1F0E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F87'), Title = MakeString('\u1F8F'), Upper = MakeString('\u1F0F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F80'), Title = MakeString('\u1F88'), Upper = MakeString('\u1F08','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F81'), Title = MakeString('\u1F89'), Upper = MakeString('\u1F09','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F82'), Title = MakeString('\u1F8A'), Upper = MakeString('\u1F0A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F83'), Title = MakeString('\u1F8B'), Upper = MakeString('\u1F0B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F84'), Title = MakeString('\u1F8C'), Upper = MakeString('\u1F0C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F85'), Title = MakeString('\u1F8D'), Upper = MakeString('\u1F0D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F86'), Title = MakeString('\u1F8E'), Upper = MakeString('\u1F0E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F87'), Title = MakeString('\u1F8F'), Upper = MakeString('\u1F0F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F90'), Title = MakeString('\u1F98'), Upper = MakeString('\u1F28','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F91'), Title = MakeString('\u1F99'), Upper = MakeString('\u1F29','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F92'), Title = MakeString('\u1F9A'), Upper = MakeString('\u1F2A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F93'), Title = MakeString('\u1F9B'), Upper = MakeString('\u1F2B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F94'), Title = MakeString('\u1F9C'), Upper = MakeString('\u1F2C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F95'), Title = MakeString('\u1F9D'), Upper = MakeString('\u1F2D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F96'), Title = MakeString('\u1F9E'), Upper = MakeString('\u1F2E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F97'), Title = MakeString('\u1F9F'), Upper = MakeString('\u1F2F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F90'), Title = MakeString('\u1F98'), Upper = MakeString('\u1F28','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F91'), Title = MakeString('\u1F99'), Upper = MakeString('\u1F29','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F92'), Title = MakeString('\u1F9A'), Upper = MakeString('\u1F2A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F93'), Title = MakeString('\u1F9B'), Upper = MakeString('\u1F2B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F94'), Title = MakeString('\u1F9C'), Upper = MakeString('\u1F2C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F95'), Title = MakeString('\u1F9D'), Upper = MakeString('\u1F2D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F96'), Title = MakeString('\u1F9E'), Upper = MakeString('\u1F2E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1F97'), Title = MakeString('\u1F9F'), Upper = MakeString('\u1F2F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA0'), Title = MakeString('\u1FA8'), Upper = MakeString('\u1F68','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA1'), Title = MakeString('\u1FA9'), Upper = MakeString('\u1F69','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA2'), Title = MakeString('\u1FAA'), Upper = MakeString('\u1F6A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA3'), Title = MakeString('\u1FAB'), Upper = MakeString('\u1F6B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA4'), Title = MakeString('\u1FAC'), Upper = MakeString('\u1F6C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA5'), Title = MakeString('\u1FAD'), Upper = MakeString('\u1F6D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA6'), Title = MakeString('\u1FAE'), Upper = MakeString('\u1F6E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA7'), Title = MakeString('\u1FAF'), Upper = MakeString('\u1F6F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA0'), Title = MakeString('\u1FA8'), Upper = MakeString('\u1F68','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA1'), Title = MakeString('\u1FA9'), Upper = MakeString('\u1F69','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA2'), Title = MakeString('\u1FAA'), Upper = MakeString('\u1F6A','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA3'), Title = MakeString('\u1FAB'), Upper = MakeString('\u1F6B','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA4'), Title = MakeString('\u1FAC'), Upper = MakeString('\u1F6C','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA5'), Title = MakeString('\u1FAD'), Upper = MakeString('\u1F6D','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA6'), Title = MakeString('\u1FAE'), Upper = MakeString('\u1F6E','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FA7'), Title = MakeString('\u1FAF'), Upper = MakeString('\u1F6F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FB3'), Title = MakeString('\u1FBC'), Upper = MakeString('\u0391','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FB3'), Title = MakeString('\u1FBC'), Upper = MakeString('\u0391','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FC3'), Title = MakeString('\u1FCC'), Upper = MakeString('\u0397','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FC3'), Title = MakeString('\u1FCC'), Upper = MakeString('\u0397','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FF3'), Title = MakeString('\u1FFC'), Upper = MakeString('\u03A9','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FF3'), Title = MakeString('\u1FFC'), Upper = MakeString('\u03A9','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FB2'), Title = MakeString('\u1FBA','\u0345'), Upper = MakeString('\u1FBA','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FB4'), Title = MakeString('\u0386','\u0345'), Upper = MakeString('\u0386','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FC2'), Title = MakeString('\u1FCA','\u0345'), Upper = MakeString('\u1FCA','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FC4'), Title = MakeString('\u0389','\u0345'), Upper = MakeString('\u0389','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FF2'), Title = MakeString('\u1FFA','\u0345'), Upper = MakeString('\u1FFA','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FF4'), Title = MakeString('\u038F','\u0345'), Upper = MakeString('\u038F','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FB7'), Title = MakeString('\u0391','\u0342','\u0345'), Upper = MakeString('\u0391','\u0342','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FC7'), Title = MakeString('\u0397','\u0342','\u0345'), Upper = MakeString('\u0397','\u0342','\u0399') }, 
      new CaseMap { Lower = MakeString('\u1FF7'), Title = MakeString('\u03A9','\u0342','\u0345'), Upper = MakeString('\u03A9','\u0342','\u0399') }, 
      new CaseMap { Key = '\u03A3', Lower = MakeString('\u03C2'), Title = MakeString('\u03A3'), Upper = MakeString('\u03A3') },
    };

    #endregion


    //class Tree<K, V> where V : class
    //{
    //  class Node
    //  {
    //    public V Value;
    //    public readonly Dictionary<K, Node> Children = new Dictionary<K, Node>();
    //  }

    //  readonly Node root = new Node();

    //  public V Partial;

    //  int depth = 0;

    //  public int Depth { get { return depth; } }

    //  public V this[params K[] keys]
    //  {
    //    get
    //    {
    //      if (keys.Length > depth)
    //      {
    //        return null;
    //      }

    //      Node c = root;

    //      for (int i = 0; i < keys.Length; i++)
    //      {
    //        var k = keys[i];
    //        Node n;
    //        if (!c.Children.TryGetValue(k, out n))
    //        {
    //          return null;
    //        }

    //        c = n;
    //      }

    //      return c.Value;
    //    }
    //    set
    //    {
    //      depth = System.Math.Max(depth, keys.Length);

    //      Node c = root;

    //      for (int i = 0; i < keys.Length; i++)
    //      {
    //        var k = keys[i];
    //        Node n;
    //        if (!c.Children.TryGetValue(k, out n))
    //        {
    //          c.Children[k] = n = new Node { Value = Partial };
    //        }

    //        c = n;
    //      }

    //      c.Value = value;
    //    }
    //  }

    //}

    //static readonly Tree<char, CaseMap> LowerCase = new Tree<char, CaseMap>() { Partial = CaseMap.Partial };
    //static readonly Tree<char, CaseMap> UpperCase = new Tree<char, CaseMap>() { Partial = CaseMap.Partial };
    //static readonly Tree<char, CaseMap> TitleCase = new Tree<char, CaseMap>() { Partial = CaseMap.Partial };
    static Hashtable Mapping = new Hashtable();

    static Unicode()
    {

      foreach (var cm in mappings)
      {
        Mapping[cm.Key] = cm;
        //LowerCase[cm.Lower] = cm;
        //UpperCase[cm.Title] = cm;
        //TitleCase[cm.Upper] = cm;
      }
    }


  }
}


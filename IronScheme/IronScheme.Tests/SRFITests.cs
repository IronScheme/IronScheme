using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class SRFI : TestRunner
  {
    [Test]
    public void AndLet()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\and-let%2a.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 36 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void CharSets()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\char-sets.sps");
      Assert.True(r.Output.Contains(";; *** failed ***"));
      Assert.True(r.Output.Contains("test: (is char-set= char-set:digit)"));
      AssertError(r);
    }

    [Test]
    public void CompareProcedures()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\compare-procedures.sps");
      Assert.True(r.Output.Contains("*** correct examples: 99590"));
      Assert.True(r.Output.Contains("*** wrong examples:   0"));
      AssertError(r);
    }

    [Test]
    public void Cut()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\cut.sps");
      Assert.True(r.Output.Contains(";; *** checks *** : 30 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void EagerComprehensions()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\eager-comprehensions.sps");
      Assert.True(r.Output.Contains("correct examples : 162"));
      Assert.True(r.Output.Contains("wrong examples   : 1"));
      AssertError(r);
    }

    [Test]
    public void IntermediateFormatStrings()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\intermediate-format-strings.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 95 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void LightweightTesting()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lightweight-testing.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 9 correct, 4 failed. First failed example:"));
      AssertError(r);
    }

    [Test]
    public void ListQueues()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\list-queues.sps");
      Assert.True(r.Output.Contains(@"%%%% Starting test list-queues/simple
# of expected passes      16
%%%% Starting test list-queues/whole
# of expected passes      20
%%%% Starting test list-queues/map
# of expected passes      23
%%%% Starting test list-queues/conversion
# of expected passes      30
%%%% Starting test list-queues/unfold
# of expected passes      34"));
      AssertError(r);
    }

    [Test]
    public void Lists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lists.sps");
      Assert.True(r.Output == "Done.");
      AssertError(r);
    }

    [Test]
    public void MultiDimensionalArraysArlib()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays--arlib.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 47 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void MultiDimensionalArrays()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 24 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void OSEnvironmentVariables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\os-environment-variables.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 4 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void ProcedureArity()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\procedure-arity.sps");
      Assert.True(r.Output.Length == 0);
      AssertError(r);
    }

    [Test]
    public void R6RSHashtables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\r6rs-hashtables.sps");
      Assert.True(r.Output.Contains("# of expected passes      179"));
      Assert.True(r.Output.Contains("# of unexpected failures  7")); // eqv? hashtable exposes hash function
      AssertError(r);
    }

    [Test]
    public void RandomAccessLists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-access-lists.sps");
      Assert.True(r.Output.Length == 0);
      AssertError(r);
    }

    [Test]
    public void RandomBits()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-bits.sps");
      Assert.False(r.Output.Contains("failed"));
      AssertError(r);
    }

    [Test]
    public void Rec()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\rec.sps");
      Assert.True(r.Output.Contains("3628800"));
      AssertError(r);
    }

    [Test]
    public void Records()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\records.sps");
      Assert.True(r.Output.Contains("; *** checks *** : 11 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void Regexp()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\regexp.sps");
      Assert.False(r.Output.Contains("*** failed ***"));
      AssertError(r);
    }

    [Test]
    public void TablesTest()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\tables-test.sps");
      Assert.False(r.Output.Contains("Error: test failed:"));
      //AssertError(r); // spits out warnings
    }

    [Test]
    public void Testing()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\testing.sps");
      Assert.True(r.Output.Contains("# of expected passes      51"));
      Assert.True(r.Output.Contains("# of expected failures    2"));
      AssertError(r);
    }

    [Test]
    public void Time()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\time.sps");
      Assert.True(r.Output.Contains(";;; Results: Runs: 8; Goods: 8; Bads: 0; Pass rate: 1"));
      AssertError(r);
    }

    [Test]
    public void Vectors()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\vectors.sps");
      Assert.True(r.Output == "Done.");
      AssertError(r);
    }
  }
}

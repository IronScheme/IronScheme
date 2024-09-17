using NUnit.Framework;

namespace IronScheme.Tests
{
  public class SRFI : TestRunner
  {
    [Test]
    public void AndLet()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\and-let%2a.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 36 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void Ascii()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\ascii.sps");
      Assert.That(r.Output, Is.Empty);
      AssertError(r);
    }

    [Test]
    public void CharSets()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\char-sets.sps");
      Assert.That(r.Output, Is.StringContaining(";; *** failed ***"));
      Assert.That(r.Output, Is.StringContaining("test: (is char-set= char-set:digit)"));
      AssertError(r);
    }

    [Test]
    public void CompareProcedures()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\compare-procedures.sps");
      Assert.That(r.Output, Is.StringContaining("*** correct examples: 99590"));
      Assert.That(r.Output, Is.StringContaining("*** wrong examples:   0"));
      AssertError(r);
    }

    [Test]
    public void Cut()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\cut.sps");
      Assert.That(r.Output, Is.StringContaining(";; *** checks *** : 30 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void EagerComprehensions()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\eager-comprehensions.sps");
      Assert.That(r.Output, Is.StringContaining("correct examples : 162"));
      Assert.That(r.Output, Is.StringContaining("wrong examples   : 1"));
      AssertError(r);
    }

    [Test]
    public void IntermediateFormatStrings()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\intermediate-format-strings.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 95 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void LightweightTesting()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lightweight-testing.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 9 correct, 4 failed. First failed example:"));
      AssertError(r);
    }

    [Test]
    public void ListQueues()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\list-queues.sps");
      Assert.That(r.Output, Is.StringContaining(@"%%%% Starting test list-queues/simple
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
      Assert.That(r.Output, Is.EqualTo("Done."));
      AssertError(r);
    }

    [Test]
    public void MultiDimensionalArraysArlib()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays--arlib.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 47 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void MultiDimensionalArrays()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 24 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void OSEnvironmentVariables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\os-environment-variables.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 4 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void ProcedureArity()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\procedure-arity.sps");
      Assert.That(r.Output, Is.Empty);
      AssertError(r);
    }

    [Test]
    public void R6RSHashtables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\r6rs-hashtables.sps");
      Assert.That(r.Output, Is.StringContaining("# of expected passes      179"));
      Assert.That(r.Output, Is.StringContaining("# of unexpected failures  7")); // eqv? hashtable exposes hash function
      AssertError(r);
    }

    [Test]
    public void RandomAccessLists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-access-lists.sps");
      Assert.That(r.Output, Is.Empty);
      AssertError(r);
    }

    [Test]
    public void RandomBits()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-bits.sps");
      Assert.That(r.Output, Is.Not.StringContaining("failed"));
      AssertError(r);
    }

    [Test]
    public void Rec()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\rec.sps");
      Assert.That(r.Output, Is.StringContaining("3628800"));
      AssertError(r);
    }

    [Test]
    public void Records()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\records.sps");
      Assert.That(r.Output, Is.StringContaining("; *** checks *** : 11 correct, 0 failed."));
      AssertError(r);
    }

    [Test]
    public void Regexp()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\regexp.sps");
      Assert.That(r.Output, Is.Not.StringContaining("*** failed ***"));
      AssertError(r);
    }

    [Test]
    public void TablesTest()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\tables-test.sps");
      Assert.That(r.Output, Is.Not.StringContaining("Error: test failed:"));
      //AssertError(r); // spits out warnings
    }

    [Test]
    public void Testing()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\testing.sps");
      Assert.That(r.Output, Is.StringContaining("# of expected passes      51"));
      Assert.That(r.Output, Is.StringContaining("# of expected failures    2"));
      AssertError(r);
    }

    [Test]
    public void Time()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\time.sps");
      Assert.That(r.Output, Is.StringContaining(";;; Results: Runs: 8; Goods: 8; Bads: 0; Pass rate: 1"));
      AssertError(r);
    }

    [Test]
    public void Vectors()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\vectors.sps");
      Assert.That(r.Output, Is.EqualTo("Done."));
      AssertError(r);
    }
  }
}

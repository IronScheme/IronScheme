using Xunit;


namespace IronScheme.Tests
{
  public class SRFIxUnit1 : TestRunnerxUnit
  {
    [Fact]
    public void AndLet()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\and-let%2a.sps");
      Assert.Contains("; *** checks *** : 36 correct, 0 failed.", r.Output);
      AssertError(r);
    }

  }
  public class SRFIxUnit2 : TestRunnerxUnit
  {

    [Fact]
    public void Ascii()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\ascii.sps");
      Assert.Empty(r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit3 : TestRunnerxUnit
  {
    [Fact]
    public void CharSets()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\char-sets.sps");
      Assert.Contains(";; *** failed ***", r.Output);
      Assert.Contains("test: (is char-set= char-set:digit)", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit4 : TestRunnerxUnit
  {
    [Fact]
    public void CompareProcedures()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\compare-procedures.sps");
      Assert.Contains("*** correct examples: 99590", r.Output);
      Assert.Contains("*** wrong examples:   0", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit5 : TestRunnerxUnit
  {
    [Fact]
    public void Cut()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\cut.sps");
      Assert.Contains(";; *** checks *** : 30 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit6 : TestRunnerxUnit
  {
    [Fact]
    public void EagerComprehensions()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\eager-comprehensions.sps");
      Assert.Contains("correct examples : 162", r.Output);
      Assert.Contains("wrong examples   : 1", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit7 : TestRunnerxUnit
  {
    [Fact]
    public void IntermediateFormatStrings()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\intermediate-format-strings.sps");
      Assert.Contains("; *** checks *** : 95 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit8 : TestRunnerxUnit
  {

    [Fact]
    public void LightweightTesting()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lightweight-testing.sps");
      Assert.Contains("; *** checks *** : 9 correct, 4 failed. First failed example:", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit9 : TestRunnerxUnit
  {
    [Fact]
    public void ListQueues()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\list-queues.sps");
      Assert.Contains(@"%%%% Starting test list-queues/simple
# of expected passes      16
%%%% Starting test list-queues/whole
# of expected passes      20
%%%% Starting test list-queues/map
# of expected passes      23
%%%% Starting test list-queues/conversion
# of expected passes      30
%%%% Starting test list-queues/unfold
# of expected passes      34", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit10 : TestRunnerxUnit
  {
    [Fact]
    public void Lists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lists.sps");
      Assert.Contains("Done.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit11 : TestRunnerxUnit
  {
    [Fact]
    public void MultiDimensionalArraysArlib()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays--arlib.sps");
      Assert.Contains("; *** checks *** : 47 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit12 : TestRunnerxUnit
  {
    [Fact]
    public void MultiDimensionalArrays()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays.sps");
      Assert.Contains("; *** checks *** : 24 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit13 : TestRunnerxUnit
  {
    [Fact]
    public void OSEnvironmentVariables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\os-environment-variables.sps");
      Assert.Contains("; *** checks *** : 4 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit14 : TestRunnerxUnit
  {
    [Fact]
    public void ProcedureArity()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\procedure-arity.sps");
      Assert.Empty(r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit15 : TestRunnerxUnit
  {
    [Fact]
    public void R6RSHashtables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\r6rs-hashtables.sps");
      Assert.Contains("# of expected passes      179", r.Output);
      Assert.Contains("# of unexpected failures  7", r.Output); // eqv? hashtable exposes hash function
      AssertError(r);
    }
  }
  public class SRFIxUnit16 : TestRunnerxUnit
  {
    [Fact]
    public void RandomAccessLists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-access-lists.sps");
      Assert.Empty(r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit17 : TestRunnerxUnit
  {
    [Fact]
    public void RandomBits()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-bits.sps");
      Assert.DoesNotContain("failed", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit18 : TestRunnerxUnit
  {
    [Fact]
    public void Rec()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\rec.sps");
      Assert.Contains("3628800", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit19 : TestRunnerxUnit
  {
    [Fact]
    public void Records()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\records.sps");
      Assert.Contains("; *** checks *** : 11 correct, 0 failed.", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit20 : TestRunnerxUnit
  {
    [Fact]
    public void Regexp()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\regexp.sps");
      Assert.DoesNotContain("*** failed ***", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit21 : TestRunnerxUnit
  {
    [Fact]
    public void TablesTest()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\tables-test.sps");
      Assert.DoesNotContain("Error: test failed:", r.Output);
      //AssertError(r); // spits out warnings
    }
  }
  public class SRFIxUnit22 : TestRunnerxUnit
  {
    [Fact]
    public void Testing()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\testing.sps");
      Assert.Contains("# of expected passes      51", r.Output);
      Assert.Contains("# of expected failures    2", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit23 : TestRunnerxUnit
  {
    [Fact]
    public void Time()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\time.sps");
      Assert.Contains(";;; Results: Runs: 8; Goods: 8; Bads: 0; Pass rate: 1", r.Output);
      AssertError(r);
    }
  }
  public class SRFIxUnit24 : TestRunnerxUnit
  {
    [Fact]
    public void Vectors()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\vectors.sps");
      Assert.Contains("Done.", r.Output );
      AssertError(r);
    }
  }
}

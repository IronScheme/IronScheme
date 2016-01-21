using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class SRFITests : TestRunner
  {
    [Test]
    public void AndLet()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\and-let_.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void CompareProcedures()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\compare-procedures.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Cut()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\cut.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void EagerComprehensions()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\eager-comprehensions.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void IntermediateFormatStrings()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\intermediate-format-strings.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void LightweightTesting()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lightweight-testing.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Lists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\lists.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void MultiDimentionalArraysArlib()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays--arlib.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void MultiDimentionalArrays()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\multi-dimensional-arrays.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void OSEnvironmentVariables()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\os-environment-variables.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void ProcedureArity()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\procedure-arity.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void PrintASCII()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\print-ascii.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Random()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void RandomAccessLists()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\random-access-lists.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void RecFactorial()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\rec-factorial.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Records()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\records.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Testing()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\testing.sps");
      Console.WriteLine(r.Output);
    }

    [Test]
    public void Time()
    {
      var r = RunIronSchemeTest(@"lib\srfi\tests\time.sps");
      Console.WriteLine(r.Output);
    }
  }
}

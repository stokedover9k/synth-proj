Project Management and Build
============================

This project is managed and built using Scala Build Tool (SBT). If you do not have it, you can find out how to easily set it up [here][setupsbt].

[setupsbt]:http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

Start SBT (in the project root directory - one containing "build.sbt"):
$ sbt
This might take a little while to bring in the dependency libraries.

Specifications
==============

The specifications are located in the *src/test/scala* directory.

The Living Specs extend the LivingSpec class (*src/test/scala/util/LivingSpec.scala*).  They rely on CSV which are exported from the course spreadsheet specifications and should be placed in *src/test/resources*.

The following are some of the Living Specs (in *src/test/scala/synth/*):

+ HarmonicSeriesLivingSpec.scala

	> Ensures that the Harmonic series is computed correctly.

+ PythagoreanSeriesLivingSpec.scala

	> Ensures that the Pythagorean series is computed correctly.

+ EvenTempSeriesLivingSpec

	> Ensures that the Even Temperment series is computed correctly.

+ scales/ModesLivingSpec.scala

	> Ensures that the Pythagorean series modes are computed correctly.

+ scales/FullPythagScLivingSpec.scala

	> Ensures that the full scale (14 notes) are created correctly using the Pythagorean series.

+ scales/PtolemyScLivingSpec.scala

	> Ensures that the Ptolemy scale interval adjustments are computed correctly.

+ scales/DodecophonicPythagScLivingSpec.scala

	> Ensures that the Dodecophonic scales is computed correctly.

+ scales/CentsHarmonicLivingSpec.scala

	> Ensures that the Harmonic series shows the note names based on the closest intervals of the Pythagorean series.

+ scales/CentsPythagHeptotonicLivingSpec.scala

	> Ensures that the Pythagorean series displays correct cents values.

Running the Specifications
--------------------------

To run a particular (set of) specification(s), execute in sbt

		test-only specification_name

Specification names can use wild cards.  For example, to run specifications for all series, execute in sbt

		test-only *Series*

To run all the specifications in the *src/test/* root, execute in sbt

		test

Demo Programs
=============

To run a specific specific class with a main, execute in sbt

		run-main class_name

Here are some of the demos:

+	synth.SeriesRunner
	
	> Displays different series and their computation computation as a table.

+	synth.ScalePlayer

	> Plays scales first as a mode of the original scale and then as a scale reconstructed from the mode's first frequency.

+	gui.DemoShowScales

	> A GUI app that shows and plays several scales.

Run them with no arguments to see the usage message.

For example, to display the series computation table, run the class in *src/main/scala/synth/SeriesRunner.scala*. You can execute in in sbt like this:

		run-main synth.SeriesRunner h 123.4

Where h indicates the Harmonic series and 123.4 is the frequency of the fundamental.  To show the computation of the Pythagorean series, replace "h" with "p".

Project Structure
=================

All the content discussed in this section is in the *src/main/scala* directory.

The *synth* directory contains the music related classes and the *util* directory contains utility classes and libraries implemented for use in synth.

*util/expr* contains a library for mathematical expressions primarily implemented to represent the frequency-generating functions for the note series (see the abstract class Interval in *synth/Series.scala*).

package sbn.core.io.filereaders

import sbn.core.CustomSpec

class ARFFDataFileReaderSpec extends CustomSpec {

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if the ARFF file is empty" in {
    val exception = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/empty.arff"))

    assert(exception.isInstanceOf[IllegalArgumentException] &&
          (exception.getMessage equals "ARFF file does not start with a @relation line"))

  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if the ARFF file does not contain a @relation line" in {
    val exception = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/noRelation.arff"))

    assert(exception.isInstanceOf[IllegalArgumentException] &&
          (exception.getMessage equals "ARFF file does not start with a @relation line"))

  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if the ARFF file does not contain @attribute lines" in {
    val exception = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/noAttributes.arff"))

    assert(exception.isInstanceOf[IllegalArgumentException] &&
          (exception.getMessage equals "ARFF File does not contain @attribute lines"))
  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if there are repeated attributes" in {
    val exception = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/repeatedAttributes.arff"))

    assert(exception.isInstanceOf[IllegalArgumentException] &&
          (exception.getMessage equals "requirement failed: Attribute names cannot be repeated"))

  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if there is a badly formatted @attribute line" in {
    val exception1 = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/badlyFormattedAttribute1.arff"))
    val exception2 = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/badlyFormattedAttribute2.arff"))
    // Right now returns an ArrayIndexOutOfBoundsException, but should return a IllegalArgumentException (another try-catch)
    val exception3 = shouldFail(ARFFDataFileReader.loadImmutableDataSet("datasets/test/core/io/fileReaders/ARFFDataFileReaderSpec/badlyFormattedAttribute3.arff"))

    assert(exception1.isInstanceOf[IllegalArgumentException] &&
          (exception1.getMessage equals "Not able to create an attribute from this line: @attribute var1 [0,1]"))

    assert(exception2.isInstanceOf[IllegalArgumentException] &&
          (exception2.getMessage equals "The number of columns does not match the number of attributes"))

    assert(exception3.isInstanceOf[ArrayIndexOutOfBoundsException])
  }
}

package ferjorosa.sbn.core.io

import ferjorosa.sbn.core.CustomSpec

/**
 * Created by Fernando on 31/10/2016.
 */
class DataFileLoaderSpec extends CustomSpec{

  "DataFileLoader" should "return a Failure if the file's extension is not supported" in {
    val exception1 = shouldFail(DataFileLoader.loadImmutableDataSet("datasets/test/core/io/DataFileLoaderSpec/fileExtensionNotSupported"))
    val exception2 = shouldFail(DataFileLoader.loadImmutableDataSet("datasets/test/core/io/DataFileLoaderSpec/fileExtensionNotSupported.lol"))

    assert(exception1.isInstanceOf[IllegalArgumentException].&(exception1.getMessage equals "File extension not supported"))
    assert(exception2.isInstanceOf[IllegalArgumentException].&(exception2.getMessage equals "File extension not supported"))
  }

  it should "return a Failure if the provided path is a directory" in {
    val exception = shouldFail(DataFileLoader.loadImmutableDataSet("datasets/test/core/io/DataFileLoaderSpec"))

    assert(exception.isInstanceOf[IllegalArgumentException]
      .&(exception.getMessage equals "The path refers to a directory, which is not supported yet"))
  }

}

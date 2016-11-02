package ferjorosa.sbn.core.io.filereaders

import ferjorosa.sbn.core.CustomSpec

/**
 * Created by Fernando on 31/10/2016.
 */
class ARFFDataFileReaderSpec extends CustomSpec {

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if the ARFF file does not contain a @relation line" in {

  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if the ARFF file does not contain @attribute lines" in {

  }

  "ARFFDataFileReader.loadImmutableDataSet" should "return a Failure if there is a badly formatted @attribute line" in {

  }
}

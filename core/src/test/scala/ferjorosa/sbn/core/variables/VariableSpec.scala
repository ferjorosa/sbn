package ferjorosa.sbn.core.variables

import ferjorosa.sbn.core.CustomSpec
import ferjorosa.sbn.core.data.attributes.{FiniteStateSpace, ManifestAttribute, RealStateSpace}

class VariableSpec extends CustomSpec{

  "VariableFactory.newMultinomialVariable" should "throw an IllegalArgumentException if its attribute's state space is not finite" in {
    val manifestAttribute = ManifestAttribute("attr", RealStateSpace())
    a[IllegalArgumentException] should be thrownBy{
      VariableFactory.newMultinomialVariable(manifestAttribute)
    }
  }

  "VariableFactory.newMultinomialVariable" should "return a new multinomial ManifestVariable when a correct ManifestAttribute is provided" in {
    val manifestAttribute = ManifestAttribute("attr", FiniteStateSpace(10))
    val multinomialManifestVariable = VariableFactory.newMultinomialVariable(manifestAttribute)
  }

  "VariableFactory.newMultinomialVariable" should "return a new multinomial LatentVariable when a correct configuration is provided" in {
    val multinomialLatentVariable = VariableFactory.newMultinomialVariable("multinomialLatentVariable", 4)

    assert(multinomialLatentVariable.isInstanceOf[LatentVariable])
    assert(multinomialLatentVariable.distributionType.isInstanceOf[MultinomialType])
    assert(!multinomialLatentVariable.id.equals(null))
  }

  "VariableFactory.newGaussianVariable" should "throw an IllegalArgumentException if its attribute's state space is not real" in {
    val manifestAttribute = ManifestAttribute("attr", FiniteStateSpace(2))
    a[IllegalArgumentException] should be thrownBy{
      VariableFactory.newGaussianVariable(manifestAttribute)
    }
  }

  "VariableFactory.newGaussianVariable" should "return a new gaussian ManifestVariable when a correct ManifestAttribute is provided" in {
    val manifestAttribute = ManifestAttribute("attr", RealStateSpace())
    val gaussianManifestVariable = VariableFactory.newGaussianVariable(manifestAttribute)

  }

  "VariableFactory.newGaussianVariable" should "return a new gaussian LatentVariable when a correct configuration is provided" in {
    val gaussianLatentVariable1 = VariableFactory.newGaussianVariable("gaussianLatentVariable1")
    val gaussianLatentVariable2 = VariableFactory.newGaussianVariable("gaussianLatentVariable2", 0, 1)

    assert(gaussianLatentVariable1.isInstanceOf[LatentVariable] && gaussianLatentVariable2.isInstanceOf[LatentVariable])
    assert(gaussianLatentVariable1.distributionType.isInstanceOf[GaussianType] && gaussianLatentVariable2.distributionType.isInstanceOf[GaussianType])
    assert(!gaussianLatentVariable1.id.equals(null) && !gaussianLatentVariable1.id.equals(null))
  }

}

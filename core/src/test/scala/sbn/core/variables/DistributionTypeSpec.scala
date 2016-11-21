package sbn.core.variables

import sbn.core.CustomSpec

class DistributionTypeSpec extends CustomSpec{

  "MultinomialType.isParentCompatible" should "return true if the parent is also MultinomialType" in {
    val multinomialTypeChild = new MultinomialType
    val multinomialTypeParent = new MultinomialType

    assert(multinomialTypeChild.isParentCompatible(multinomialTypeParent))
  }

  "MultinomialType.isParentCompatible" should "return false otherwise" in {
    val multinomialTypeChild = new MultinomialType
    val gaussianTypeParent = new GaussianType

    assert(!multinomialTypeChild.isParentCompatible(gaussianTypeParent))
  }

  "GaussianType.isParentCompatible" should "return false always" in {
    val gaussianTypeChild = new GaussianType
    val multinomialTypeParent = new MultinomialType
    val gaussianTypeParent = new GaussianType

    assert(!gaussianTypeChild.isParentCompatible(multinomialTypeParent))
    assert(!gaussianTypeChild.isParentCompatible(gaussianTypeParent))
  }
}

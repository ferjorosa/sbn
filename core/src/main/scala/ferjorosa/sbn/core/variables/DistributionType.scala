package ferjorosa.sbn.core.variables

/**
  * Represents the univariate distribution type of a variable.
  */
trait DistributionType{

  /**
    * Tests whether a given parent is compatible or not. In other words, it checks if the resulting conditional distribution
    * would be allowed by the library or not.
    * @param distributionType the parent's distribution type.
    * @return true if the parent is compatible, false otherwise.
    */
  def isParentCompatible(distributionType: DistributionType): Boolean
}

/**
  * This class represents a multinomial distribution type.
  */
class MultinomialType extends DistributionType{

  /** @inheritdoc */
  def isParentCompatible(distributionType: DistributionType): Boolean = distributionType match {
      // resulting distribution: Multinomial_MultinomialParents
      case multinomial: MultinomialType => true
      case _ => false
  }

}

/**
  * This class represents a gaussian distribution type.
  */
class GaussianType extends DistributionType{

  /** @inheritdoc */
  def isParentCompatible(distributionType: DistributionType): Boolean = false
}

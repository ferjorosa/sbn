package sbn.core.variables

import java.util.UUID

import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace, StateSpaceType}

/**
  * The [[Variable]] factory. It is designed to create manifest and latent variables in a different way. Manifest variables
  * are created from a [[Attribute]], which comes from a DataSource, while Latent variables are created by the user,
  * specifying its parameters.
  */
object ModelVariablesFactory {

  /**
    * Creates a manifest multinomial variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not finite.
    * @return a new [[ManifestVariable]] of multinomial type.
    */
  @throws[IllegalArgumentException]
  def newMultinomialVariable(attribute: Attribute): ManifestVariable = ManifestVariable(attribute, new MultinomialType, UUID.randomUUID())

  /**
    * Creates a latent multinomial variable by specifying its number of states.
    *
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate multinomial distribution.
    * @return a new [[LatentVariable]] of multinomial type.
    */
  def newMultinomialVariable(name: String, nStates: Int): LatentVariable = {
    val attribute = Attribute(name, FiniteStateSpace(nStates))
    LatentVariable(attribute, new MultinomialType, UUID.randomUUID())
  }

  /**
    * Creates a manifest gaussian variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not real.
    * @return a new [[ManifestVariable]] of gaussian type.
    */
  @throws[IllegalArgumentException]
  def newGaussianVariable(attribute: Attribute): ManifestVariable = ManifestVariable(attribute, new GaussianType, UUID.randomUUID())

  /**
    * Creates a latent gaussian variable by specifying its value intervals.
    *
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String, min: Double, max: Double): LatentVariable = {
    val attribute = Attribute(name, RealStateSpace(min, max))
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a latent gaussian variable with inifinite value intervals.
    *
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String): LatentVariable = {
    val attribute = Attribute(name, RealStateSpace())
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

}

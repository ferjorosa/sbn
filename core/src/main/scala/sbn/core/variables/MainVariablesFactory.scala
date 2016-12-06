package sbn.core.variables

import java.util.UUID

import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace, StateSpaceType}

/**
  * The [[MainVariable]] factory. It is designed to create manifest and latent variables in a different way. Manifest variables
  * are created from a [[Attribute]], which comes from a DataSource, while Latent variables are created by the user,
  * specifying its parameters.
  */
object MainVariablesFactory {

  /**
    * Creates a manifest multinomial variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not finite.
    * @return a new [[ManifestVariable]] of multinomial type.
    */
  @throws[IllegalArgumentException]
  def newMultinomialMV(attribute: Attribute): MainVariable = ManifestVariable(attribute, new MultinomialType, UUID.randomUUID())

  /**
    * Creates a latent multinomial variable by specifying its number of states.
    *
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate multinomial distribution.
    * @return a new [[LatentVariable]] of multinomial type.
    */
  def newMultinomialLV(name: String, nStates: Int): MainVariable = {
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
  def newGaussianMV(attribute: Attribute): MainVariable = ManifestVariable(attribute, new GaussianType, UUID.randomUUID())

  /**
    * Creates a latent gaussian variable by specifying its value intervals.
    *
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianLV(name: String, min: Double, max: Double): MainVariable = {
    val attribute = Attribute(name, RealStateSpace(min, max))
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a latent gaussian variable with inifinite value intervals.
    *
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianLV(name: String): MainVariable = {
    val attribute = Attribute(name, RealStateSpace())
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

}

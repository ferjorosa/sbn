package ferjorosa.sbn.core.variables

import java.util.UUID

import ferjorosa.sbn.core.data.attributes.Attribute

/**
 * This class represents a manifes variable.
 */
class ManifestVariable(attribute: Attribute, name: String, id: UUID) {
  var isObservable: Boolean = false
}

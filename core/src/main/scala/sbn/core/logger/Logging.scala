package sbn.core.logger

import com.typesafe.scalalogging.Logger

/**
 * Logging is a trait you can mix in to provide easy log4j logging for your scala classes.
 */
trait Logging {
    val loggerName = this.getClass.getName
    lazy val logger = Logger(loggerName)
}

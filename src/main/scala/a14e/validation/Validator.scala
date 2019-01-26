package a14e.validation

import a14e.validation.engines.MutableRulesEngine


trait Validator[INPUT, OUT] extends MutableRulesEngine[INPUT, OUT]
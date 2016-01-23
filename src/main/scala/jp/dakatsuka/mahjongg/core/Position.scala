package jp.dakatsuka.mahjongg.core

sealed trait Position
case object Jicha extends Position
case object Simocha extends Position
case object Toimen extends Position
case object Kamicha extends Position

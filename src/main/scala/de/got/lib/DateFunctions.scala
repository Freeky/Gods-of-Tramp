package de.got.lib
import java.text.SimpleDateFormat
import java.util.Locale

object DateFunctions {
  val germanShortDate = new SimpleDateFormat("dd.MM.", Locale.GERMAN)
  val germanDate = new SimpleDateFormat("dd.MM.yyyy", Locale.GERMAN)
  val germanTime = new SimpleDateFormat("HH:mm", Locale.GERMAN)
  val germanShortDateTime = new SimpleDateFormat("dd.MM. HH:mm", Locale.GERMAN)
  val germanDateTime = new SimpleDateFormat("dd.MM.yyyy HH:mm", Locale.GERMAN)

  def formatShortDate(d: java.util.Date): String = d match { case null => "" case s => germanShortDate.format(s) }
  def formatDate(d: java.util.Date): String = d match { case null => "" case s => germanDate.format(s) }
  def formatTime(d: java.util.Date): String = d match { case null => "" case s => germanTime.format(s) }
  def formatShortDateTime(d: java.util.Date): String = d match { case null => "" case s => germanShortDateTime.format(s) }
  def formatDateTime(d: java.util.Date): String = d match { case null => "" case s => germanDateTime.format(s) }
}
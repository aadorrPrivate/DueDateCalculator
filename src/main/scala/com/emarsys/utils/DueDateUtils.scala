package com.emarsys.utils

import org.joda.time.{Duration, DateTime}

/**
  * Created by u95425 on 2016.02.10.
  */
object DueDateUtils {

  private val dayShift = Duration.standardHours(16)
  private val weekendShift = Duration.standardDays(2)

  def remainingWorkhours(currentDateTime: DateTime): Duration = {
    val endOfWorkDay = currentDateTime.withHourOfDay(17).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)
    return new Duration(currentDateTime, endOfWorkDay)
  }

  def shift (currentDate: DateTime) : Duration = currentDate match {
    case cd if(currentDate.plusDays(1).getDayOfWeek > 5 ) => weekendShift.plus(dayShift)
    case _ => dayShift
  }
}

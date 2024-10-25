package com.emarsys.utils

import java.time.{Duration, LocalDateTime}

/**
 * DueDateUtils object containing utility methods to calculate due dates based on reporting date and turnaround time.
 */
object DueDateUtils {

  private val dayShift = Duration.ofHours(16)
  private val weekendShift = Duration.ofDays(2)

  /**
   * Calculate the remaining workhours from the current date until the end of the workday
   * @param currentDateTime
   * @return
   */
  def remainingWorkhours(currentDateTime: LocalDateTime): Duration = {
    val endOfWorkDay = currentDateTime.withHour(17).withMinute(0).withSecond(0).withNano(0)
    Duration.between(currentDateTime, endOfWorkDay)
  }

  /**
   * Calculate the shift based on the current date
   * At the end of working hours we shift to the next day - to 9am
   * At the end of the week we shift to the next week - to Monday 9am
   * @param currentDate
   * @return
   */
  def shift (currentDate: LocalDateTime) : Duration = currentDate match {
    case cd if currentDate.plusDays(1).getDayOfWeek.getValue > 5  => weekendShift.plus(dayShift)
    case _ => dayShift
  }
}

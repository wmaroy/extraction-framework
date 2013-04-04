package org.dbpedia.extraction.config.dataparser


object DurationParserConfig
{

    // TODO: get this map from data types configuration
    val timesMap = Map(
        "en" -> Map(
            "second" -> "second",
            "s" -> "second",
            "sec" -> "second",
            "seconds" -> "second",
            "secs" -> "second",
            "minute" -> "minute",
            "m" -> "minute",
            "min" -> "minute",
            "minutes" -> "minute",
            "min." -> "minute",
            "mins" -> "minute",
            "minu" -> "minute",
            "hour" -> "hour",
            "h" -> "hour",
            "hour" -> "hour",
            "hr" -> "hour",
            "hr." -> "hour",
            "hrs" -> "hour",
            "hrs." -> "hour",
            "day" -> "day",
            "d" -> "day",
            "d." -> "day",
            "days" -> "day",
            "month" -> "month",
            "months" -> "month",
            "year" -> "year",
            "y" -> "year",
            "years" -> "year",
            "yr" -> "year"
        ),
        // For "ar" configuration, rendering right-to-left may seems like a bug, but it's not.
        // Don't change this else if you know how it is done.
        "ar" -> Map(
          "ثانية" -> "second",
          "ثا" -> "second",
          "ثواني" -> "second",
          "دقيقة" -> "minute",
          "د" -> "minute",
          "دقائق" -> "minute",
          "ساعة" -> "hour",
          "سا" -> "hour",
          "ساعات" -> "hour",
          "يوم" -> "day",
          "أيام" -> "day",
          "شهر" -> "month",
          "شهور" -> "month",
          "سنة" -> "year",
          "عام" -> "year",
          "سنوات" -> "year",
          "أعوام" -> "year",
          "حول" -> "year"
        ),
        "de" -> Map(
            "Sekunde" -> "second",
            "Sekunden" -> "second",
            "sekunde" -> "second",
            "sekunden" -> "second",
            "sek" -> "second",
            "Sek" -> "second",
            "s" -> "second",
            "Minute" -> "minute",
            "Minuten" -> "minute",
            "minuten" -> "minute",
            "m" -> "minute",
            "min" -> "minute",
            "min." -> "minute",
            "mins" -> "minute",
            "Stunde" -> "hour",
            "Stunden" -> "hour",
            "stunde" -> "hour",
            "stunden" -> "hour",
            "std" -> "hour",
            "Std" -> "hour",
            "std." -> "hour",
            "Std." -> "hour",
            "h" -> "hour",
            "Tag" -> "day",
            "Tage" -> "day",
            "tag" -> "day",
            "tage" -> "day",
            "Monat" -> "month",
            "Monate" -> "month",
            "monat" -> "month",
            "monate" -> "month",
            "Jahr" -> "year",
            "Jahre" -> "year",
            "jahr" -> "year",
            "jahre" -> "year"
        ),
        "el" -> Map(
            "δευτερόλεπτο" -> "second",
            "δευτερολεπτο" -> "second",
            "δευτερόλεπτα" -> "second",
            "δευτερολεπτα" -> "second",
            "δεύτερα" -> "second",
            "δ" -> "second",
            "δδ" -> "second",
            "λεπτό" -> "minute",
            "λεπτο" -> "minute",
            "λεπτά" -> "minute",
            "λεπτα" -> "minute",
            "λ" -> "minute",
            "λλ" -> "minute",
            "ώρα" -> "hour",
            "ωρα" -> "hour",
            "ώρες" -> "hour",
            "ωρες" -> "hour",
            "ω" -> "hour",
            "ωω" -> "hour",
            "ημέρα" -> "day",
            "ημερα" -> "day",
            "ημέρες" -> "day",
            "ημερες" -> "day",
            "η" -> "day",
            "ηη" -> "day",
            "μήνας" -> "month",
            "μήνα" -> "month",
            "μήνες" -> "month",
            "μηνας" -> "month",
            "μηνα" -> "month",
            "μηνες" -> "month",
            "μ" -> "month",
            "μμ" -> "month",
            "χρόνος" -> "year",
            "χρόνοι" -> "year",
            "χρόνια" -> "year",
            "χρονος" -> "year",
            "χρονοι" -> "year",
            "χρονια" -> "year",
            "έτος" -> "year",
            "έτη" -> "year",
            "ετος" -> "year",
            "ετη" -> "year",
            "ε" -> "year",
            "εε" -> "year"
        ),
        "nl" -> Map(
            "Seconde" -> "second",
            "Seconden" -> "second",
            "seconde" -> "second",
            "seconden" -> "second",
            "sec" -> "second",
            "Sec" -> "second",
            "s" -> "second",
            "Minuut" -> "minute",
            "minuut" -> "minute",
            "minuten" -> "minute",
            "Minuten" -> "minute",
            "m" -> "minute",
            "min" -> "minute",
            "min." -> "minute",
            "mins" -> "minute",
            "Uur" -> "hour",
            "uur" -> "hour",
            "Uren" -> "hour",
            "uren" -> "hour",
            "h" -> "hour",
            "Dag" -> "day",
            "Dagen" -> "day",
            "dag" -> "day",
            "dagen" -> "day",
            "Maand" -> "month",
            "Maanden" -> "month",
            "maand" -> "month",
            "maanden" -> "month",
            "Jaar" -> "year",
            "Jaren" -> "year",
            "jaar" -> "year",
            "jr" -> "year",
            "jr." -> "year",
            "jaren" -> "year"
        ),
        "pt" -> Map(
            "segundo" -> "second",
            "segundos" -> "second",
            "seg" -> "second",
            "segs" -> "second",
            "minuto" -> "minute",
            "minutos" -> "minute",
            "hora" -> "hour",
            "horas" -> "hour",
            "dia" -> "day",
            "dias" -> "day",
            "mes" -> "month",
            "meses" -> "month",
            "mês" -> "month",
            "ano" -> "year",
            "anos" -> "year",
            "año" -> "year",
            "años" -> "year"
        ),
        "es" -> Map(
            "segundo" -> "second",
            "segundos" -> "second",
            "seg" -> "second",
            "segs" -> "second",
            "minuto" -> "minute",
            "minutos" -> "minute",
            "min" -> "minute",
            "mins" -> "minute",
            "hora" -> "hour",
            "horas" -> "hour",
            "dia" -> "day",
            "dias" -> "day",
            "día" -> "day",
            "días" -> "day",
            "mes" -> "month",
            "meses" -> "month",
            "año" -> "year",
            "años" -> "year"
        )
    )
}

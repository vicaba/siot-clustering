import config.Configuration

(1 to 100).toList.grouped(36).toList.foreach( l => println(l.size))


Configuration.summaryBatchRunFile.split('.').head
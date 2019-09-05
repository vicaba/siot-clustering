val order = List(33, 22, 11, 55)

val works = List((33, "some text"), (55, "eeeee"), (22, "fdsfs"), (11, "fdsffds"))

val worksMap = works.toMap

val newWorks = order zip order.map(worksMap)
val pointRegex = """-p(\d+)-""".r

println(pointRegex.findFirstMatchIn("-p123123123-DISHWASHERR").get.group(1))

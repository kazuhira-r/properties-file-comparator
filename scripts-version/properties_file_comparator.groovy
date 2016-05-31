native2AsciiCommand = 'native2ascii -encoding %s %s'
propertiesFileEncoding = 'UTF-8'
propertiesFileSuffix = '.utf8'

def path1 = args[0]
def path2 = args[1]

def propertiesFile1 = autoNative2Ascii(path1)
def propertiesFile2 = autoNative2Ascii(path2)

compareProperties(propertiesFile1.clone(), propertiesFile2.clone())

def compareProperties(properties1, properties2) {
    if (properties1.isEmpty()) {
        def removes = []
        for (entry2 in properties2) {
            def prop2 = new Property(key: entry2.key, value: entry2.key)
            removes << entry2.key
            printDifference(new Property(), prop2)
        }

        removes.each { k -> properties2.remove(k) }
    } else if (properties2.isEmpty()) {
        def removes = []
        for (entry1 in properties1) {
            def prop1 = new Property(key: entry1.key, value: properties1[entry1.key])
            removes << entry1.key
            printDifference(prop1, new Property())
        }

        removes.each { k -> properties1.remove(k) }
    } else {
        def key1 = properties1.firstKey()
        def key2 = properties2.firstKey()

        if (key1 > key2) {
            def prop2 = new Property(key: key2, value: properties2.remove(key2))
            printDifference(new Property(), prop2)
            compareProperties(properties1, properties2)
        } else if (key1 < key2) {
            def prop1 = new Property(key: key1, value: properties1.remove(key1))
            printDifference(prop1, new Property())
            compareProperties(properties1, properties2)
        } else {
            def value1 = properties1.remove(key1)
            def value2 = properties2.remove(key2)

            if (value1 != value2) {
                def prop1 = new Property(key: key1, value: value1)
                def prop2 = new Property(key: key2, value: value2)
                printDifference(prop1, prop2)
            }

            compareProperties(properties1, properties2)
        }
    }
}

def printDifference(prop1, prop2) {
    if (prop1.hasKey()) {
        println("< $prop1.key = ${prop1.printableValue()}")
    }

    if (prop2.hasKey()) {
        println("> $prop2.key = ${prop2.printableValue()}")
    }

    println("---")
}

def requireNative2Ascii(fileName) {
    propertiesFileSuffix != null && fileName.endsWith(propertiesFileSuffix)
}

def autoNative2Ascii(fileName) {
    if (requireNative2Ascii(fileName)) {
        def command = String.format(native2AsciiCommand, propertiesFileEncoding, fileName)
        def proc = command.execute()
        def outputStream = new ByteArrayOutputStream()
        proc.consumeProcessOutput(outputStream, new ByteArrayOutputStream())

        def retVal = proc.waitFor()

        if (retVal != 0) {
            throw new IllegalArgumentException("native2ascii ret = [${retVal}]")
        }

        def properties = new Properties()
        new ByteArrayInputStream(outputStream.toByteArray()).withStream { properties.load(it) }
        new TreeMap(properties)
    } else {
        def properties = new Properties()
        new File(fileName).withInputStream { properties.load(it) }
        new TreeMap(properties)
    }
}

class Property {
    def key
    def value

    def hasKey() { key != null }
    def printableValue() {
        if (value == null) {
            ""
        } else {
            def builder = new StringBuilder()
            for (v in value) {
                switch (v) {
                    case '\n':
                        builder.append("\\n")
                        break
                    case '\r':
                        builder.append("\\r")
                        break
                    case '\t':
                        builder.append("\\t")
                        break
                    default:
                        builder.append(v)
                        break
                }
            }
            builder.toString()
        }
    }
}

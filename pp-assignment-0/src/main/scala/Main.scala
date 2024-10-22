package pp202402.assign0

object Main:
  val studentID: String = "2021-18641"
  val studentName: String = "이하동"

  val laptopOS: String = "Windows"
  val laptopCPU: String = "11th Gen Intel(R) Core(TM) i5-1135G7"

  def isValidOS(os: String): Boolean =
    if os == "Windows" || os == "MacOS" || os == "Linux" || os == "None" then
      true
    else false

  def splitDashFromID(id: String): (String, String) =
    val splitted = id.split("-")
    (splitted(0), splitted(1))

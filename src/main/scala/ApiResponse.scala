case class ApiResponse(
                        database: String,
                        table: String,
                        data: Data,
                        nextLink: Option[String]
                      )

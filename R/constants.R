#' CONSTANTS
#'
#' Helper object to store constants such as endpoint addresses and such.
CONSTANTS <- list(
  ENDPOINTS = list(
    JOB = "job",
    JOBS = "jobs",
    VERSION = "version",
    ADDRESS = "address",
    JOB_CANCEL = "job/cancel",
    PING = "ping"
  ),
  ERRORS = list(
    UNAUTHORIZED = "rsq_unauthorized",
    SERVER = "rsq_server_error",
    SERVICE_UNRESPONSIVE = "rsq_service_unresponsive"
  ),
  CLASSES = list(
    API_CONFIG = "rsq_api_config",
    RSQ_JOB = "rsq_job"
  ),
  JOB_STATUS = list(
    QUEUED = "QUEUED",
    RUNNING = "RUNNING",
    COMPLETED = "COMPLETED",
    ERROR = "ERROR"
  )
)

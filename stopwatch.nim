
#proc getPerformanceCounter(): uint64
#proc getPerformanceFrequency(): uint64

type
  StopWatch* = object
    value : int64  ## when running it's the offset, otherwise, it's the timer when it was stopped the last time
    running: bool

proc reset*(this: var StopWatch): void =
  ## seths the timer to zero
  this.value = getPerformanceCounter().int64

proc newStopWatch*(running: bool; startTime: float64 = 0.0) : StopWatch =
  result.value   = getPerformanceCounter().int64 - int64(startTime * getPerformanceFrequency().float64)
  result.running = running
  
proc toggle*(this: var StopWatch): void =
  this.running = not this.running
  this.value = getPerformanceCounter().int64 - this.value

proc cont*(this: var StopWatch): void =
  ## continue is keyword, therefore just cont
  if not this.running:
    this.toggle

proc stop*(this: var StopWatch): void =
  if this.running:
    this.toggle

proc time*(this: StopWatch): float64 =
  ## returns time in seconds
  let counter = 
    if this.running:
      getPerformanceCounter().int64 - this.value
    else:
      this.value

  float64(counter) / float64(getPerformanceFrequency())

proc running*(this: StopWatch): bool = this.running
  

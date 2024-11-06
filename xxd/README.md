### program flow

reads from stdin OR reads from file
converts to bytes and displays in hexadecimal format 16 bytes per line.
something like: 

`'{offset} {hex} {ascii representation?}'`


## to do

figure out what right side representation actually is and when  `.` is used instead
of an ascii character

xxd uses some color coding on the output that I don't quite understand yet
`\xff` becomes blue, while other bytes are normal,red, or green.


### performance monitoring

#### time output on this clone
```
Command being timed: "./xxd ../../gogames/goflappy/assets/bgm.ogg"
	User time (seconds): 0.10
	System time (seconds): 0.25
	Percent of CPU this job got: 93%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.39
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 1696
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 84
	Voluntary context switches: 306
	Involuntary context switches: 34
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
```

```
Command being timed: "xxd -R never ../../gogames/goflappy/assets/bgm.ogg"
	User time (seconds): 0.09
	System time (seconds): 0.24
	Percent of CPU this job got: 91%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.36
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 1584
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 83
	Voluntary context switches: 356
	Involuntary context switches: 33
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
```

.PHONY: 2024-01-1
2024-01-1:
	sbcl --script 2024/day-01-1.lisp

.PHONY: 2024-01-2
2024-01-2:
	sbcl --script 2024/day-01-2.lisp

.PHONY: 2024-02-1
2024-02-1:
	cc 2024/day-02-1.c -o output/2024-day-02-1
	./output/2024-day-02-1

.PHONY: 2024-02-2
2024-02-2:
	cc 2024/day-02-2.c -o output/2024-day-02-2
	./output/2024-day-02-2

.PHONY: 2024-03-1
2024-03-1:
	snobol4 2024/day-03-1.sno < 2024/input/day-03.txt

.PHONY: 2024-03-2
2024-03-2:
	snobol4 2024/day-03-2.sno < 2024/input/day-03.txt

.PHONY: 2024-04-1
2024-04-1:
	ruby 2024/day-04-1.rb

.PHONY: 2024-04-2
2024-04-2:
	ruby 2024/day-04-2.rb

.PHONY: 2024-05-1
2024-05-1:
	swipl -g main -t halt 2024/day-05-1.prolog

.PHONY: 2024-05-2
2024-05-2:
	swipl -g main -t halt 2024/day-05-2.prolog

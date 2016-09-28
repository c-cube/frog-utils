
#OASIS_START
#OASIS_STOP

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.3; \
		make ; \
	done

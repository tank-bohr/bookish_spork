.PHONY: all clean
.DEFAULT_GOAL := all

PEMS=key.pem cert.pem
CONFIG=ssl.conf

all: $(PEMS)

$(PEMS):
	openssl req -config $(CONFIG) -new -x509 -sha256 -newkey rsa:2048 -nodes -keyout key.pem -days 3650 -out cert.pem

clean:
	rm -f $(PEMS)

REPO ?= alb42/amitubeserver
TAG  ?= latest
NAME ?= amitubeserver
pwd="${PWD}"


.PHONY: build buildnc run push logs clean release bash run-localstorage

default: build

build:
	docker build -t $(REPO):$(TAG) .

buildnc:
	docker build --no-cache -t $(REPO):$(TAG) .

run:
	docker run -d -it --rm -p 1234:80 --name $(NAME) $(REPO):$(TAG)

run-localstorage:
	mkdir -p $(pwd)/data/logs
ifeq ("$(wildcard $(pwd)/data/ytshares.xml)","")
	echo "<?xml version=\"1.0\"?><results></results>" > $(pwd)/data/ytshares.xml
endif
	-chmod -R a+rwx $(pwd)/data
	docker run -d --rm \
		-v $(pwd)/data/logs:/var/log/amitube \
		-v $(pwd)/data/ytshares.xml:/var/www/html/ytshares.xml \
		-p 1234:80 \
		--name $(NAME) \
		$(REPO):$(TAG)

stop:
	docker kill $(NAME)

bash:
	docker exec -it $(NAME) /bin/bash

push:
	docker push $(REPO):$(TAG)

logs:
	docker logs $(NAME)

clean:
	docker rmi -f $(REPO):$(TAG)

release: buildnc push

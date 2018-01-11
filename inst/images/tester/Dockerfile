FROM r-base

RUN apt-get update && \
        apt-get install -yy \
                git \
                libcurl4-openssl-dev \
                libssl-dev \
                qpdf

RUN install2.r --error \
        curl \
        knitr \
        openssl \
        rcmdcheck \
        rmarkdown \
        rcorpora \
        testthat \
        uuid

COPY tester.sh /usr/local/bin
RUN chmod +x /usr/local/bin/tester.sh

ENTRYPOINT ["/usr/local/bin/tester.sh"]

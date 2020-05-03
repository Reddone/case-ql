FROM hseeberger/scala-sbt:8u252_1.3.10_2.12.11

RUN mkdir -p /opt/case-ql-build
RUN mkdir -p /opt/case-ql-example

WORKDIR /opt/case-ql-build
COPY . .
RUN sbt dist
RUN cp modules/example/target/universal/case-ql-example-0.1.0.zip /opt/case-ql-example

WORKDIR ../case-ql-example
RUN rm -rf /opt/case-ql-build
RUN unzip case-ql-example-0.1.0.zip && mv case-ql-example-0.1.0 stage

EXPOSE 4000

ENTRYPOINT ["/bin/bash", "stage/bin/case-ql-example"]
CMD []

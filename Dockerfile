ARG VERSION
ARG JAVA_VERSION
ARG SBT_VERSION
ARG SCALA_VERSION

FROM hseeberger/scala-sbt:${JAVA_VERSION}_${SBT_VERSION}_${SCALA_VERSION}

RUN mkdir -p /opt/case-ql-build
RUN mkdir -p /opt/case-ql-example

WORKDIR /opt/case-ql-build
COPY . .
RUN sbt dist
RUN cp modules/example/target/universal/case-ql-example-${VERSION}.zip /opt/case-ql-example

WORKDIR ../case-ql-example
RUN rm -rf /opt/case-ql-build
RUN unzip case-ql-example-${VERSION}.zip && mv case-ql-example-${VERSION} stage

EXPOSE 4000

ENTRYPOINT ["/bin/bash", "stage/bin/case-ql-example"]
CMD []

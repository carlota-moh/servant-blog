FROM kilna/liquibase-postgres
ENV LIQUIBASE_HOST=postgres
ENV LIQUIBASE_PORT=5435
ENV LIQUIBASE_DATABASE=warehouse
ENV LIQUIBASE_USERNAME=postgres
ENV LIQUIBASE_PASSWORD=password
ENV LIQUIBASE_CHANGELOG=changelog.yaml
COPY migrations/changelog.yaml /workspace
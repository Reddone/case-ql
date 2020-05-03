CREATE TABLE IF NOT EXISTS public.developer (
    id             BIGSERIAL    PRIMARY KEY,
    name           VARCHAR(255) NOT NULL,
    age            INTEGER      NOT NULL,
    team_leader_id BIGINT       NULL REFERENCES public.developer (id) ON UPDATE CASCADE ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS developer_idx ON public.developer (team_leader_id);

CREATE TABLE IF NOT EXISTS public.project (
    id          BIGSERIAL    PRIMARY KEY,
    title       VARCHAR(255) NOT NULL,
    description TEXT         NULL
);

CREATE TABLE IF NOT EXISTS public.developer_project_link (
    developer_id BIGINT NOT NULL REFERENCES public.developer (id) ON UPDATE CASCADE ON DELETE CASCADE,
    project_id   BIGINT NOT NULL REFERENCES public.project (id)   ON UPDATE CASCADE ON DELETE CASCADE,
    PRIMARY KEY (developer_id, project_id)
);

CREATE INDEX IF NOT EXISTS developer_project_link_idx ON public.developer_project_link (project_id);

CREATE TABLE IF NOT EXISTS public.task (
    id          BIGSERIAL    PRIMARY KEY,
    label       VARCHAR(255) NOT NULL,
    description TEXT         NULL DEFAULT 'EAZY PEAZY',
    deadline    TIMESTAMP    NOT NULL,
    project_id  BIGINT       NOT NULL REFERENCES public.project (id) ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS task_idx ON public.task (project_id);

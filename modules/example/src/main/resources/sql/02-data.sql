INSERT INTO public.developer (name, age, team_leader_id) VALUES
('name1', 1, NULL),
('name2', 2, NULL);

INSERT INTO public.project (title, description) VALUES
('title1', NULL),
('title2', NULL);

INSERT INTO public.developer_project_link (developer_id, project_id) VALUES
(1, 1),
(2, 2);

INSERT INTO public.task (label, description, deadline, project_id) VALUES
('label1', NULL, '2014-07-02 06:14:00', 1),
('label2', NULL, '2014-07-02 06:14:00', 2);

INSERT INTO public.developer (name, age, team_leader_id) VALUES
('name1', 32, NULL),
('name2', 1, 1),
('name3', 1, 1),
('name4', 23, NULL),
('name5', 2, 4),
('name6', 2, 4),
('name7', 42, NULL),
('name8', 3, 7);

INSERT INTO public.project (title, description) VALUES
('title1', 'description1'),
('title2', NULL),
('title3', 'description3');

INSERT INTO public.developer_project_link (developer_id, project_id) VALUES
(1, 1),
(2, 1),
(3, 1),
(5, 1),
(4, 2),
(5, 2),
(6, 2),
(3, 2),
(7, 3),
(8, 3);

INSERT INTO public.task (label, description, deadline, project_id) VALUES
('label1', 'description1', '2020-01-01 01:01:01', 1),
('label2', 'description2', '2020-02-02 02:02:02', 1),
('label3', NULL, '2020-01-01 01:01:01', 2),
('label4', NULL, '2020-02-02 02:02:02', 2),
('label5', 'description5', '2020-03-03 03:03:03', 3),
('label6', NULL, '2020-03-03 03:03:03', 3);

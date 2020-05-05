INSERT INTO public.developer (name, age, team_leader_id) VALUES
('Shabaz Delarosa', 32, NULL),
('Mathilda Luna', 19, 1),
('Mikolaj Carney', 19, 1),
('Zakir Dennis', 23, NULL),
('Russell Whitaker', 27, 4),
('Shakira Mcdonnell', 27, 4),
('Catrin Mcgowan', 42, NULL),
('Zeenat Mitchell', 35, 7);

INSERT INTO public.project (title, description) VALUES
('Move to Kubernetes', 'Do as everyone does'),
('Rewrite everything from scratch', NULL),
('Burn the Mainframe', 'Find a way to create a better world');

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
('Install the cluster', 'Follow the tutorial', '2020-01-01 01:01:01', 1),
('Test the cluster', 'Follow another tutorial', '2020-02-02 02:02:02', 1),
('Delete old code', NULL, '2020-01-01 01:01:01', 2),
('Reinvent the wheel', NULL, '2020-02-02 02:02:02', 2),
('Mine some bitcoins', 'We need to have something to spend at the black market', '2020-03-03 03:03:03', 3),
('Buy a nuclear warhead', NULL, '2020-03-03 03:03:03', 3);

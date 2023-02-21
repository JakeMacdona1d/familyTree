from graphviz import Digraph

class Person:
    def __init__(self, name, parent=None):
        self.name = name
        self.parent = parent
        self.children = []

    def add_child(self, child):
        self.children.append(child)
        child.parent = self

    def get_ancestors(self):
        ancestors = []
        parent = self.parent
        while parent is not None:
            ancestors.append(parent)
            parent = parent.parent
        return ancestors

    def get_descendants(self):
        descendants = []
        for child in self.children:
            descendants.append(child)
            descendants.extend(child.get_descendants())
        return descendants

    def __str__(self):
        return self.name

def create_family_tree():
    families = [
        (['tom_smith', 'evelyn_harris'], ['mark_smith', 'freddy_smith', 'joe_smith', 'francis_smith']),
        (['mark_smith', 'pam_wilson'], ['martha_smith', 'frederick_smith']),
        (['freddy_smith', 'connie_warrick'], ['jill_smith', 'marcus_smith', 'tim_smith']),
        (['john_smith', 'layla_morris'], ['julie_smith', 'leslie_smith', 'heather_smith', 'zach_smith']),
        (['edward_thompson', 'susan_holt'], ['leonard_thompson', 'mary_thompson']),
        (['leonard_thompson', 'list_smith'], ['joe_thompson', 'catherine_thompson', 'john_thompson', 'carrie_thompson']),
        (['joe_thompson', 'lisa_houser'], ['lilly_thompson', 'richard_thompson', 'marcus_thompson']),
        (['john_thompson', 'mary_snyder'], []),
        (['jeremiah_leech', 'sally_swithers'], ['arthur_leech']),
        (['arthur_leech', 'jane_smith'], ['timothy_leech', 'jack_leech', 'heather_leech']),
        (['robert_harris', 'julia_swift'], ['evelyn_harris', 'albert_harris']),
        (['albert_harris', 'margaret_little'], ['june_harris', 'jackie_harrie', 'leonard_harris']),
        (['leonard_harris', 'constance_may'], ['jennifer_harris', 'karen_harris', 'kenneth_harris']),
        (['beau_morris', 'jennifer_willis'], ['layla_morris']),
        (['willard_louis', 'missy_deas'], ['jonathan_louis']),
        (['jonathan_louis', 'marsha_lang'], ['tom_louis']),
        (['tom_louis', 'catherine_thompson'], ['mary_louis', 'jane_louis', 'katie_louis'])
    ]

    people = {}
    for parents, children in families:
        parent_objs = []
        for parent_name in parents:
            if parent_name not in people:
                parent = Person(parent_name)
                people[parent_name] = parent
            else:
                parent = people[parent_name]
            parent_objs.append(parent)
        for child_name in children:
            if child_name not in people:
                child = Person(child_name)
                people[child_name] = child
            else:
                child = people[child_name]
            for parent in parent_objs:
                parent.add_child(child)

    dot = Digraph(comment='Family Tree')
    for person in people.values():
        dot.node(person.name, person.name)

    for person in people.values():
        for parent in person.get_ancestors():
            dot.edge(parent.name, person.name)

    dot.render('family_tree.gv', view=True)  # save and view the diagram


create_family_tree()
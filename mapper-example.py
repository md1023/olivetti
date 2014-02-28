class Base(object):

    @classmethod
    def get_by_id(cls, id):
        return Session.query(cls).filter_by(id).one()


class SuperBase(Base):
    pass

class MegaBase(Base):
    pass


mapper(Base, table_base)
mapper(SuperBase, table_super_base)
mapper(MegaBase, table_mega_base)


......


obj = SuperBase.get_by_id(...)

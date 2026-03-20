from dbload.config.parser import get_odbc_dict

def using_prod_db():
    """
    Check if using prod_db, return True if so, False otherwise
    Parse server name (e.g. FILEPATH) for dev
    """
    if (get_odbc_dict("ushd_db")['server'].split("-")[1] == "dev"):
        prod = False
    elif (get_odbc_dict("ushd_db")['server'].split("-")[1] == "unmanaged"):
        print("You should update your ~/.ODBC.ini")
        if (get_odbc_dict("ushd_db")['server'].split("-")[3][0] == "p"):
            prod = True
        else:
            prod = False
    else:
        prod = True
    return(prod)
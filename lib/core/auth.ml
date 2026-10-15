type login_request = { first_name : string; last_name : string; email : string }

let login { first_name; last_name; email } =
  {
    first_name = String.uppercase_ascii first_name;
    last_name = String.uppercase_ascii last_name;
    email = String.uppercase_ascii email;
  }

import { ErrorPage } from "@/views/common/ErrorPage";

const NotFound = () => {
  return (
    <ErrorPage
      tittle={404}
      description="The page you are attempting to reach is currently not
      available.This may be because the page does not exist or has been
      moved."
      tittleClassName="font-danger"
      BtnClassName="btn-danger-gradien"
    />
  );
};

export default NotFound;

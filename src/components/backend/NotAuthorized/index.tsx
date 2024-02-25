import { ErrorPage } from "@/views/common/ErrorPage";

export const NotAuthorized = () => {
  return (
    <ErrorPage
      tittle={401}
      description="The page you are attempting to reach is currently not
      available for your account."
      tittleClassName="font-warning"
      BtnClassName="btn-warning-gradien"
    />
  );
};

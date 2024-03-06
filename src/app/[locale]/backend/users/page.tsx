import { getTranslations } from "next-intl/server";
import { Card, CardBody, Col, Container, Row } from "reactstrap";

import { Breadcrumbs } from "@/components/backend/Breadcrumbs";
import { AclGuard } from "@/components/backend/Guards/AclGuard";
import CommonCardHeading from "@/components/common/CommonCardHeading";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("users"));

  return {
    title,
  };
}

export default async function UsersPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = t("users");
  const pageTitle = t("listName", { name: t("users") });
  const pageDescription = t("seeInformationAboutName", {
    name: t("users").toLowerCase(),
  });

  return (
    <AclGuard acl={{ action: "Read", subject: "User" }}>
      <div className="page-body">
        <Breadcrumbs title={title} pageTitle={title} />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card>
                <CommonCardHeading
                  smallHeading={pageTitle}
                  span={pageDescription}
                />
                <CardBody>
                  <p>
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
                    do eiusmod tempor incididunt ut labore et dolore magna
                    aliqua. Ut enim ad minim veniam, quis nostrud exercitation
                    ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    Duis aute irure dolor in reprehenderit in voluptate velit
                    esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
                    occaecat cupidatat non proident, sunt in culpa qui officia
                    deserunt mollit anim id est laborum.
                  </p>
                </CardBody>
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  );
}

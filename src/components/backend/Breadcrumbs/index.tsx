import React from "react";
import { Col, Container, Row } from "reactstrap";

import SvgIcon from "@/components/common/Icons/SvgIcon";
import { Link } from "@/navigation";

type BreadcrumbProps = {
  title: string;
  mainTitle: string;
  parent?: string;
  subParent?: string;
};

export const Breadcrumbs = ({
  title,
  mainTitle,
  parent,
  subParent,
}: BreadcrumbProps) => {
  return (
    <div>
      <Container fluid={true}>
        <div className="page-title">
          <Row>
            <Col xs={6} className="p-0">
              <h4>{mainTitle}</h4>
            </Col>
            <Col xs={6} className="p-0">
              <ol className="breadcrumb">
                <li className="breadcrumb-item">
                  <Link href={`/dashboard/default`}>
                    <SvgIcon iconId="stroke-home" />
                  </Link>
                </li>
                <li className="breadcrumb-item">{parent}</li>
                {subParent ? (
                  <li className="breadcrumb-item">{subParent}</li>
                ) : (
                  ""
                )}
                <li className="breadcrumb-item active">{title}</li>
              </ol>
            </Col>
          </Row>
        </div>
      </Container>
    </div>
  );
};

"use client";

import React from "react";
import { Col, Container, Row } from "reactstrap";

import SvgIcon from "@/components/common/SvgIcon";
import { Link } from "@/navigation";

type BreadcrumbProps = {
  title?: string;
  pageTitle: string;
  parent?: string;
  subParent?: string;
};

export const Breadcrumbs = ({
  title,
  pageTitle,
  parent,
  subParent,
}: BreadcrumbProps) => {
  return (
    <div>
      <Container fluid={true}>
        <div className="page-title">
          <Row>
            <Col xs={6} className="p-0">
              <h4>{pageTitle}</h4>
            </Col>
            <Col xs={6} className="p-0">
              <ol className="breadcrumb">
                <li className="breadcrumb-item">
                  <Link href={`/backend`}>
                    <SvgIcon iconId="stroke-home" />
                  </Link>
                </li>
                {parent && <li className="breadcrumb-item">{parent}</li>}
                {subParent ? (
                  <li className="breadcrumb-item">{subParent}</li>
                ) : (
                  ""
                )}
                {title && <li className="breadcrumb-item active">{title}</li>}
              </ol>
            </Col>
          </Row>
        </div>
      </Container>
    </div>
  );
};

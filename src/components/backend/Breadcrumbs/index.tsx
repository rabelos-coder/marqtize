"use client";

import { useTranslations } from "next-intl";
import React from "react";
import { Col, Container, Row } from "reactstrap";

import SvgIcon from "@/components/common/Icons/SvgIcon";
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
  const t = useTranslations("translations");

  return (
    <div>
      <Container fluid={true}>
        <div className="page-title">
          <Row>
            <Col xs={6} className="p-0">
              <h4>{t(pageTitle)}</h4>
            </Col>
            <Col xs={6} className="p-0">
              <ol className="breadcrumb">
                <li className="breadcrumb-item">
                  <Link href={`/backend`}>
                    <SvgIcon iconId="stroke-home" />
                  </Link>
                </li>
                {parent && <li className="breadcrumb-item">{t(parent)}</li>}
                {subParent ? (
                  <li className="breadcrumb-item">{t(subParent)}</li>
                ) : (
                  ""
                )}
                {title && (
                  <li className="breadcrumb-item active">{t(title)}</li>
                )}
              </ol>
            </Col>
          </Row>
        </div>
      </Container>
    </div>
  );
};

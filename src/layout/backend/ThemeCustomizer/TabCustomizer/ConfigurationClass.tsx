import { useTranslations } from "next-intl";
import { Fragment } from "react";
import { CopyToClipboard } from "react-copy-to-clipboard";
import { toast } from "react-toastify";
import {
  Button,
  Container,
  Modal,
  ModalBody,
  ModalFooter,
  ModalHeader,
  Row,
} from "reactstrap";

import Theme from "@/configs/theme";

type PropsTypeProp = {
  toggle: () => void;
  modal: boolean;
};
const ConfigurationClass = ({ toggle, modal }: PropsTypeProp) => {
  const theme = Theme.data;
  const t = useTranslations("translations");

  return (
    <Fragment>
      <Modal
        isOpen={modal}
        toggle={toggle}
        className="modal-body"
        centered={true}
      >
        <ModalHeader toggle={toggle}>{t("configuration")}</ModalHeader>
        <ModalBody>
          <Container fluid={true} className="bd-example-row">
            <Row>
              <p>{t("configurationClassInfo")}</p>
              <p>
                <strong>
                  {t("pathDoubleDotName", { name: "src > configs > theme.ts" })}
                </strong>
              </p>
            </Row>
            <div
              style={{
                background: "#e6e6e6",
                borderRadius: "5px",
                padding: "15px",
              }}
            >
              <div>{"export class Theme "}&#123;</div>
              <div>&nbsp;&nbsp;{"static data"} = &#123;</div>
              <div>&nbsp;&nbsp;&nbsp;&nbsp;{"settings"}&#58; &#123;</div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"layout_type"}&#58; '
                {theme.settings.layout_type}',
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"layout_class"}&#58; '
                {theme.settings.layout_class}',
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"sidebar"}&#58; &#123;
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"type"}
                &#58; '{theme.settings.sidebar.type}',
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"iconType"}
                &#58; '{theme.settings.sidebar.iconType}',
              </div>
              <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#125;,</div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"sidebar_setting"}
                &#58; '{theme.settings.sidebar_setting}
                ',
              </div>
              <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#125;,</div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{"color"}&#58; &#123;
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                {"primary_color"}&#58; '{theme.color.primary_color}',
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                {"secondary_color"}&#58; '{theme.color.secondary_color}',
              </div>
              <div>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                {"mix_background_layout"}
                &#58; '{theme.color.mix_background_layout}',
              </div>
              <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#125;,</div>
              <div>&nbsp;&nbsp;&nbsp;&nbsp;&#125;</div>
              <div>&#125;</div>
            </div>
          </Container>
        </ModalBody>
        <ModalFooter>
          <CopyToClipboard text={JSON.stringify(theme)}>
            <Button
              color="primary"
              className="notification"
              onClick={() =>
                toast.success(t("codeCopied"), {
                  position: "bottom-right",
                })
              }
            >
              {t("copyText")}
            </Button>
          </CopyToClipboard>
          <Button color="secondary" onClick={toggle}>
            {t("close")}
          </Button>
        </ModalFooter>
      </Modal>
    </Fragment>
  );
};

export default ConfigurationClass;

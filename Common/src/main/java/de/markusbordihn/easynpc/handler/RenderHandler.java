/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RenderHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private RenderHandler() {}

  public static boolean setRenderType(EasyNPC<?> easyNPC, RenderType renderType) {
    if (easyNPC == null || renderType == null) {
      log.error("[{}] Error setting render type to {}", easyNPC, renderType);
      return false;
    }

    RenderData<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null) {
      log.error("[{}] No render data available for setting render type {}!", easyNPC, renderType);
      return false;
    }

    log.debug("[{}] Setting render type to {}", easyNPC, renderType);
    renderData.getRenderData().setRenderType(renderType);
    renderData.updateRenderData();
    return true;
  }

  public static boolean setRenderEntity(EasyNPC<?> easyNPC, String renderEntity) {
    if (easyNPC == null || renderEntity == null || renderEntity.isEmpty()) {
      log.error("[{}] Error setting render entity to {}", easyNPC, renderEntity);
      return false;
    }

    RenderData<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null) {
      log.error(
          "[{}] No render data available for setting render entity {}!", easyNPC, renderEntity);
      return false;
    }

    log.debug("[{}] Setting render entity to {}", easyNPC, renderEntity);
    renderData.getRenderData().setRenderEntityType(renderEntity);
    renderData.updateRenderData();
    return true;
  }
}

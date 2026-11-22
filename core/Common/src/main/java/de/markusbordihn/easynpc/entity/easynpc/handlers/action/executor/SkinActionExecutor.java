/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.handler.SkinHandler;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.UUID;

public final class SkinActionExecutor {

  private SkinActionExecutor() {}

  public static boolean setSkin(ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC) {
    if (actionDataEntry == null || easyNPC == null) {
      return false;
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    SkinDataEntry skinDataEntry = getSkinDataEntry(actionDataEntry.command(), easyNPC);
    if (skinData == null || skinDataEntry == null) {
      return false;
    }

    return SkinHandler.setSkin(
        easyNPC, skinDataEntry.withDisableLayers(skinData.getSkinDataEntry().disableLayers()));
  }

  private static SkinDataEntry getSkinDataEntry(String skinReference, EasyNPC<?> easyNPC) {
    UUID customSkinUUID = UUIDUtils.parseUUID(skinReference);
    if (customSkinUUID != null) {
      return SkinDataEntry.createCustomSkin(customSkinUUID, false);
    }

    if (easyNPC instanceof EasyNPCBase<?> easyNPCBase) {
      for (Enum<?> variant : easyNPCBase.getSkinVariantTypes()) {
        if (variant.name().equals(skinReference)) {
          return SkinDataEntry.createDefaultSkin(variant.name());
        }
      }
    }
    return null;
  }
}

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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.TextUtils;
import net.minecraft.network.chat.Style;
import net.minecraft.network.chat.TextColor;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NameHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private NameHandler() {}

  public static boolean setCustomName(EasyNPC<?> easyNPC, String name, int color, boolean visible) {
    if (easyNPC == null || name == null) {
      log.error("[{}] Error setting custom name {}", easyNPC, name);
      return false;
    }

    Entity entity = easyNPC.getEntity();

    // Remove the custom name if the name is empty.
    if (name.isEmpty()) {
      log.debug("[{}] Remove custom name", easyNPC);
      entity.setCustomName(null);
      entity.setCustomNameVisible(false);
      return true;
    }

    log.debug(
        "[{}] Change custom name to '{}' with color {} and visible {}",
        easyNPC,
        name,
        color,
        visible);

    // Define custom color and style for the name, if any.
    Style style = Style.EMPTY;
    if (color >= 0) {
      style = style.withColor(TextColor.fromRgb(color));
    }

    // Set the custom name for the entity with translation key support.
    if (TextUtils.isTranslationKey(name)) {
      entity.setCustomName(new TranslatableComponent(name).setStyle(style));
    } else {
      entity.setCustomName(new TextComponent(name).setStyle(style));
    }

    // Set the visibility of the custom name.
    entity.setCustomNameVisible(visible);

    return true;
  }
}

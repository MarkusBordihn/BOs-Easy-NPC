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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import de.markusbordihn.easynpc.client.screen.components.DrawBoxWithBorder;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;

public class PresetPreviewView {

  private PresetPreviewView() {
    // Utility class
  }

  public static void render(
      GuiGraphics guiGraphics,
      Font font,
      EasyNPC<?> npc,
      int x,
      int y,
      int width,
      int height,
      int previewY,
      int mouseX,
      int mouseY) {
    DrawBoxWithBorder.draw(guiGraphics, x, y, width, height);
    if (npc != null) {
      EntityConfigScreenRenderer.renderEntity(
          guiGraphics,
          npc,
          EntityRenderConfig.guiScaled(x + width / 2, previewY + 15, 40, 0.0f, 0.0f),
          mouseX,
          mouseY);
    } else {
      Text.drawString(
          guiGraphics,
          font,
          Component.literal("Preview not available"),
          x + 10,
          previewY,
          0x3F3F3F);
    }
  }
}

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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;

public class HelpIcon extends AbstractWidget {

  private static final int DEFAULT_SIZE = 12;
  private static final int SPRITE_X = 105;
  // The sprite sheet has the green (highlight) variant on top and the gray variant below it.
  // The muted gray variant is the resting state; hovering switches to the green highlight.
  private static final int SPRITE_Y_HOVER = 81;
  private static final int SPRITE_Y = SPRITE_Y_HOVER + DEFAULT_SIZE;

  public HelpIcon(int left, int top, Component tooltip) {
    super(left, top, DEFAULT_SIZE, DEFAULT_SIZE, Component.empty());
    this.setTooltip(Tooltip.create(tooltip));
  }

  public HelpIcon(int left, int top, String tooltipTranslationKey) {
    this(left, top, TextComponent.getTranslatedConfigText(tooltipTranslationKey));
  }

  @Override
  protected void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    guiGraphics.blit(
        Constants.TEXTURE_CONFIGURATION,
        this.getX(),
        this.getY(),
        SPRITE_X,
        this.isHovered() ? SPRITE_Y_HOVER : SPRITE_Y,
        this.width,
        this.height);
  }

  @Override
  protected void updateWidgetNarration(NarrationElementOutput narrationElementOutput) {
    narrationElementOutput.add(NarratedElementType.HINT, this.getMessage());
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    return false;
  }
}

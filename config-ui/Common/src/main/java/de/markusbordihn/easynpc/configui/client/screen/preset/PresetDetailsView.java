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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.client.screen.components.DrawBoxWithBorder;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.security.PresetFeatureNotice;
import de.markusbordihn.easynpc.security.PresetFeaturePreview;
import de.markusbordihn.easynpc.security.PresetFeatureStatus;
import de.markusbordihn.easynpc.utils.TextUtils;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.phys.Vec3;

public class PresetDetailsView {

  private static final float TEXT_SCALE = 0.8f;
  private static final String ARROW = "\u2192";

  private PresetDetailsView() {}

  public static void render(
      GuiGraphics guiGraphics,
      Font font,
      EasyNPC<?> easyNPC,
      PresetData presetData,
      PresetFeaturePreview securityPreview,
      int x,
      int y,
      int width,
      int height) {
    DrawBoxWithBorder.draw(guiGraphics, x, y, width, height);
    if (easyNPC == null) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText("preset_browser.details.no_data"),
          x + 5,
          y + 5,
          0x3F3F3F);
      return;
    }

    int scaledX = x + 5;
    int scaledY = y + 3;
    int lineHeight = 10;
    int line = 0;

    Text.drawString(
        guiGraphics,
        font,
        TextComponent.getTranslatedConfigText(
            "preset_browser.details.type", easyNPC.getEntityTypeId()),
        scaledX,
        scaledY + lineHeight * line++,
        0x3F3F3F);

    Vec3 position = presetData != null ? presetData.getPosition() : null;
    if (position != null) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.pos", TextUtils.formatPosition(position)),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (easyNPC.getEasyNPCOwnerData() != null && easyNPC.getEasyNPCOwnerData().hasNPCOwner()) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.owner", easyNPC.getEasyNPCOwnerData().getNPCOwnerName()),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (easyNPC.getEasyNPCSkinData() != null) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.skin", easyNPC.getEasyNPCSkinData().getSkinType().name()),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (securityPreview != null && securityPreview.hasNotices()) {
      line++;
      int renderedNotices = 0;
      for (PresetFeatureNotice notice : securityPreview.notices()) {
        if (notice == null || renderedNotices >= 5) {
          continue;
        }
        Text.drawString(
            guiGraphics,
            font,
            getSecurityComponent(notice),
            scaledX,
            scaledY + lineHeight * line++,
            getSecurityColor(notice.status()));
        renderedNotices++;
      }

      int remainingNotices = securityPreview.notices().size() - renderedNotices;
      if (remainingNotices > 0) {
        Text.drawString(
            guiGraphics,
            font,
            TextComponent.getTranslatedConfigText(
                "preset_browser.details.more_notices", String.valueOf(remainingNotices)),
            scaledX,
            scaledY + lineHeight * line,
            0x7F7F7F);
      }
    }
  }

  private static int getSecurityColor(PresetFeatureStatus status) {
    return switch (status) {
      case ALLOWED -> 0x00AA00;
      case BLOCKED -> 0xAA0000;
      case REDUCED -> 0xAA7700;
    };
  }

  private static Component getSecurityComponent(PresetFeatureNotice notice) {
    return switch (notice.status()) {
      case ALLOWED ->
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.notice_allowed", notice.feature().displayName());
      case BLOCKED ->
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.notice_blocked",
              Component.literal(notice.feature().displayName())
                  .withStyle(style -> style.withStrikethrough(true)));
      case REDUCED ->
          TextComponent.getTranslatedConfigText(
              "preset_browser.details.notice_reduced", getReducedFeatureText(notice));
    };
  }

  private static String getReducedFeatureText(PresetFeatureNotice notice) {
    if (notice.commandLevel() == null) {
      return notice.feature().displayName();
    }

    return notice.feature().displayName() + " " + ARROW + " " + notice.commandLevel();
  }
}

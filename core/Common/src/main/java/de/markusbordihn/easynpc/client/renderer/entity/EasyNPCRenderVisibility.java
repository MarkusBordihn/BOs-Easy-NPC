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

package de.markusbordihn.easynpc.client.renderer.entity;

import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.highlight.NPCHighlightManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.utils.ItemUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.world.entity.Entity;

public class EasyNPCRenderVisibility {

  private static final double NPC_WAND_RENDER_RANGE = 32.0d;

  private EasyNPCRenderVisibility() {}

  public static Boolean resolveShouldRenderOverride(Entity entity) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return null;
    }

    LocalPlayer player = Minecraft.getInstance().player;
    if (player == null) {
      return null;
    }

    if (NPCHighlightManager.isHighlighted(entity.getUUID())
        || isForcedVisibleByWand(entity, player)) {
      return Boolean.TRUE;
    }

    if (isHiddenFrom(easyNPC, entity, player)) {
      return Boolean.FALSE;
    }

    return null;
  }

  public static boolean isVisibleTo(Entity entity, LocalPlayer player) {
    return entity instanceof EasyNPC<?> easyNPC
        && (isForcedVisibleByWand(entity, player) || !isHiddenFrom(easyNPC, entity, player));
  }

  private static boolean isForcedVisibleByWand(Entity entity, LocalPlayer player) {
    return ItemUtils.isPlayerHoldingEasyNPCWand(player)
        && entity.distanceToSqr(player) <= NPC_WAND_RENDER_RANGE * NPC_WAND_RENDER_RANGE;
  }

  private static boolean isHiddenFrom(EasyNPC<?> easyNPC, Entity entity, LocalPlayer player) {
    return entity.isInvisible()
        || entity.isInvisibleTo(player)
        || AttributeHandler.getOpacity(easyNPC) <= DisplayAttributeType.MIN_OPACITY;
  }
}

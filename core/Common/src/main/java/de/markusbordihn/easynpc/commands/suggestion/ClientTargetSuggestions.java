/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.commands.suggestion;

import com.mojang.brigadier.Message;
import com.mojang.brigadier.context.StringRange;
import com.mojang.brigadier.suggestion.Suggestion;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

public class ClientTargetSuggestions {

  private static final double SEARCH_RANGE = 32.0;
  private static final double RAY_TOLERANCE = 1.0;
  private static final int SUGGESTION_LIMIT = 8;

  private static final int RANK_CROSSHAIR = 0;
  private static final int RANK_NEAR_CROSSHAIR = 1;
  private static final int RANK_NEARBY = 2;

  private ClientTargetSuggestions() {}

  public static List<Suggestion> suggestNPCTargets(
      StringRange stringRange, String filterPrefix, boolean showAllOwners) {
    Minecraft minecraft = Minecraft.getInstance();
    LocalPlayer localPlayer = minecraft.player;
    if (localPlayer == null) {
      return List.of();
    }

    boolean showEveryNPC = showAllOwners || localPlayer.isCreative();
    UUID localPlayerUUID = localPlayer.getUUID();
    Vec3 eyePosition = localPlayer.getEyePosition();
    Vec3 rayEnd = eyePosition.add(localPlayer.getViewVector(1.0f).scale(SEARCH_RANGE));

    return LivingEntityManager.getClientEasyNPCEntities()
        .filter(
            easyNPC ->
                easyNPC.getEntity() != null
                    && easyNPC.getEntity().isAlive()
                    && easyNPC.getEntityLevel() == localPlayer.level()
                    && easyNPC.getEntity().distanceToSqr(eyePosition)
                        <= SEARCH_RANGE * SEARCH_RANGE)
        .filter(
            easyNPC ->
                filterPrefix.isEmpty()
                    || easyNPC.getEntityUUID().toString().startsWith(filterPrefix))
        .map(
            easyNPC ->
                new RankedTarget(
                    easyNPC,
                    isOwner(easyNPC, localPlayerUUID),
                    rankOf(easyNPC, minecraft.crosshairPickEntity, eyePosition, rayEnd),
                    sortDistanceOf(easyNPC, eyePosition, rayEnd)))
        .filter(rankedTarget -> showEveryNPC || rankedTarget.owner())
        .sorted(
            Comparator.comparingInt((RankedTarget rankedTarget) -> rankedTarget.rank())
                .thenComparing(rankedTarget -> !rankedTarget.owner())
                .thenComparingDouble(RankedTarget::sortDistance))
        .limit(SUGGESTION_LIMIT)
        .map(
            rankedTarget ->
                new Suggestion(
                    stringRange,
                    rankedTarget.easyNPC().getEntityUUID().toString(),
                    createTooltip(
                        rankedTarget.easyNPC(),
                        Math.sqrt(rankedTarget.easyNPC().getEntity().distanceToSqr(eyePosition)),
                        showEveryNPC)))
        .toList();
  }

  private static boolean isOwner(EasyNPC<?> easyNPC, UUID playerUUID) {
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    return ownerData != null && ownerData.isNPCOwner(playerUUID);
  }

  private static int rankOf(
      EasyNPC<?> easyNPC, Entity crosshairPickEntity, Vec3 eyePosition, Vec3 rayEnd) {
    if (crosshairPickEntity == easyNPC.getEntity()) {
      return RANK_CROSSHAIR;
    }

    return clipViewRay(easyNPC, eyePosition, rayEnd).isPresent()
        ? RANK_NEAR_CROSSHAIR
        : RANK_NEARBY;
  }

  private static double sortDistanceOf(EasyNPC<?> easyNPC, Vec3 eyePosition, Vec3 rayEnd) {
    return clipViewRay(easyNPC, eyePosition, rayEnd)
        .map(hitPosition -> hitPosition.distanceToSqr(eyePosition))
        .orElseGet(() -> easyNPC.getEntity().distanceToSqr(eyePosition));
  }

  private static Optional<Vec3> clipViewRay(EasyNPC<?> easyNPC, Vec3 eyePosition, Vec3 rayEnd) {
    return easyNPC.getEntity().getBoundingBox().inflate(RAY_TOLERANCE).clip(eyePosition, rayEnd);
  }

  private static Message createTooltip(EasyNPC<?> easyNPC, double distance, boolean showOwner) {
    Entity entity = easyNPC.getEntity();
    MutableComponent tooltip = TextComponent.getBlankText();
    if (entity.hasCustomName()) {
      tooltip.append(entity.getName()).append(" ");
    }
    tooltip
        .append("(")
        .append(entity.getType().getDescription())
        .append(String.format(Locale.ROOT, ", %.1fm", distance));

    if (showOwner) {
      tooltip.append(", ").append(getOwnerName(easyNPC));
    }

    return tooltip.append(")");
  }

  private static String getOwnerName(EasyNPC<?> easyNPC) {
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !ownerData.hasNPCOwner()) {
      return "-";
    }

    String ownerName = ownerData.getNPCOwnerName();
    return ownerName != null && !ownerName.isEmpty()
        ? ownerName
        : ownerData.getOwnerUUID().toString().substring(0, 8);
  }

  private record RankedTarget(EasyNPC<?> easyNPC, boolean owner, int rank, double sortDistance) {}
}

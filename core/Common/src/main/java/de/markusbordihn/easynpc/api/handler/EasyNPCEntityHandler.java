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

package de.markusbordihn.easynpc.api.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEntityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EasyNPCEntityHandler() {}

  // Listing methods

  public static Collection<SavedNPCEntityEntry> getAll() {
    return NPCEntityManager.getAllNPCs();
  }

  public static Collection<SavedNPCEntityEntry> getByOwner(UUID ownerUUID) {
    return NPCEntityManager.getNPCsByOwner(ownerUUID);
  }

  public static Collection<SavedNPCEntityEntry> getByType(String entityType) {
    return NPCEntityManager.getNPCsByType(entityType);
  }

  public static Collection<SavedNPCEntityEntry> getByDimension(String dimension) {
    return NPCEntityManager.getNPCsByDimension(dimension);
  }

  public static Collection<SavedNPCEntityEntry> getByCustomIdentifier(
      ResourceLocation customIdentifier) {
    return NPCEntityManager.getNPCsByCustomIdentifier(customIdentifier);
  }

  // Despawn methods

  public static boolean despawn(EasyNPC<?> easyNPC, NPCRemovalReason reason) {
    if (easyNPC == null) {
      log.error("Cannot despawn null EasyNPC");
      return false;
    }
    NPCEntityManager.saveNPC(easyNPC);
    NPCEntityManager.updateRemovalReason(easyNPC.getEntityUUID(), reason);
    easyNPC.getEntity().discard();
    return true;
  }

  public static boolean despawn(UUID uuid, ServerLevel serverLevel, NPCRemovalReason reason) {
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC == null) {
      log.error("Cannot despawn NPC {}: not found in world", uuid);
      return false;
    }
    return despawn(easyNPC, reason);
  }

  // Spawn methods

  public static boolean spawn(UUID uuid, ServerLevel serverLevel) {
    Optional<SavedNPCEntityEntry> entry = NPCEntityManager.getNPC(uuid);
    if (entry.isEmpty()) {
      log.error("Cannot spawn NPC {}: no saved data found", uuid);
      return false;
    }
    return PresetHandler.importPreset(serverLevel, entry.get().npcData());
  }

  public static boolean spawn(UUID uuid, ServerLevel serverLevel, Vec3 position) {
    Optional<SavedNPCEntityEntry> entry = NPCEntityManager.getNPC(uuid);
    if (entry.isEmpty()) {
      log.error("Cannot spawn NPC {}: no saved data found", uuid);
      return false;
    }
    CompoundTag npcData = entry.get().npcData().copy();
    ListTag posTag = new ListTag();
    posTag.add(DoubleTag.valueOf(position.x));
    posTag.add(DoubleTag.valueOf(position.y));
    posTag.add(DoubleTag.valueOf(position.z));
    npcData.put("Pos", posTag);
    return PresetHandler.importPreset(serverLevel, npcData);
  }
}

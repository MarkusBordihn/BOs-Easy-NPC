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
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.RespawnHandler;
import de.markusbordihn.easynpc.security.CommandSecurity;
import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Predicate;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEntityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EasyNPCEntityHandler() {}

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

  public static Collection<SavedNPCEntityEntry> getByCustomIdentifierNamespace(String namespace) {
    return NPCEntityManager.getNPCsByCustomIdentifierNamespace(namespace);
  }

  public static Optional<EasyNPC<?>> find(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null || serverLevel == null) {
      log.error("Cannot look up NPC {} in level {}", uuid, serverLevel);
      return Optional.empty();
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC == null || easyNPC.getEntity().isRemoved()) {
      return Optional.empty();
    }

    return Optional.of(easyNPC);
  }

  public static Optional<EasyNPC<?>> spawnFromPreset(
      ResourceLocation preset,
      ServerLevel serverLevel,
      Vec3 position,
      UUID uuid,
      ServerPlayer owner) {
    return spawnFromPreset(PresetType.DATA, preset, serverLevel, position, uuid, owner);
  }

  public static Optional<EasyNPC<?>> spawnFromPreset(
      PresetType presetType,
      ResourceLocation preset,
      ServerLevel serverLevel,
      Vec3 position,
      UUID uuid,
      ServerPlayer owner) {
    return spawnFromPreset(presetType, preset, serverLevel, position, uuid, owner, null);
  }

  public static Optional<EasyNPC<?>> spawnFromPreset(
      ResourceLocation preset,
      ServerLevel serverLevel,
      Vec3 position,
      UUID uuid,
      ServerPlayer owner,
      ResourceLocation customIdentifier) {
    return spawnFromPreset(
        PresetType.DATA, preset, serverLevel, position, uuid, owner, customIdentifier);
  }

  public static Optional<EasyNPC<?>> spawnFromPreset(
      PresetType presetType,
      ResourceLocation preset,
      ServerLevel serverLevel,
      Vec3 position,
      UUID uuid,
      ServerPlayer owner,
      ResourceLocation customIdentifier) {
    if (preset == null || serverLevel == null) {
      log.error("Cannot spawn preset {} in level {}", preset, serverLevel);
      return Optional.empty();
    }

    return PresetHandler.importPresetAndGetEntity(
        serverLevel,
        presetType,
        preset,
        position,
        uuid,
        CommandSecurity.getServerActorContext(),
        owner,
        customIdentifier);
  }

  public static boolean despawn(EasyNPC<?> easyNPC, NPCRemovalReason reason) {
    if (easyNPC == null) {
      log.error("Cannot despawn null EasyNPC");
      return false;
    }

    NPCRemovalReason removalReason =
        reason == null || reason == NPCRemovalReason.NONE ? NPCRemovalReason.DESPAWNED : reason;
    NPCEntityManager.saveNPC(easyNPC, removalReason);
    NPCEntityManager.markIntentionalRemoval(easyNPC.getEntityUUID(), removalReason);
    easyNPC.getEntity().discard();
    return true;
  }

  public static boolean respawn(EasyNPC<?> easyNPC, ServerLevel serverLevel) {
    return RespawnHandler.respawnNPC(easyNPC, serverLevel);
  }

  public static boolean delete(EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      log.error("Cannot delete null EasyNPC");
      return false;
    }

    UUID entityUUID = easyNPC.getEntityUUID();
    NPCEntityManager.markNPCDeleted(entityUUID);
    NPCEntityManager.markIntentionalRemoval(entityUUID, NPCRemovalReason.DELETED);
    easyNPC.getEntity().discard();
    return true;
  }

  public static boolean delete(UUID uuid, ServerLevel serverLevel) {
    if (uuid == null) {
      log.error("Cannot delete NPC without a unique id");
      return false;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC != null) {
      return delete(easyNPC);
    }

    if (NPCEntityManager.getNPC(uuid).isEmpty()) {
      log.error("Cannot delete NPC {}: no saved data found", uuid);
      return false;
    }

    NPCEntityManager.markNPCDeleted(uuid);
    return true;
  }

  public static boolean despawn(UUID uuid, ServerLevel serverLevel, NPCRemovalReason reason) {
    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverLevel);
    if (easyNPC == null) {
      log.error("Cannot despawn NPC {}: not found in world", uuid);
      return false;
    }

    return despawn(easyNPC, reason);
  }

  public static boolean spawn(UUID uuid, ServerLevel serverLevel) {
    return spawn(uuid, serverLevel, false);
  }

  public static boolean spawn(UUID uuid, ServerLevel serverLevel, boolean force) {
    Optional<SavedNPCEntityEntry> entry = getSpawnableEntry(uuid, getSpawnRejection(uuid, force));
    return entry.isPresent()
        && PresetHandler.importPreset(
            serverLevel, entry.get().npcData(), entry.get().metadata().ownerUUID());
  }

  public static boolean spawn(UUID uuid, ServerLevel serverLevel, Vec3 position) {
    return spawn(uuid, serverLevel, position, false);
  }

  public static boolean spawn(UUID uuid, ServerLevel serverLevel, Vec3 position, boolean force) {
    Optional<SavedNPCEntityEntry> entry = getSpawnableEntry(uuid, getSpawnRejection(uuid, force));
    if (entry.isEmpty()) {
      return false;
    }

    CompoundTag npcData = entry.get().npcData().copy();
    putPosition(npcData, position);
    return PresetHandler.importPreset(serverLevel, npcData, entry.get().metadata().ownerUUID());
  }

  public static boolean revive(UUID uuid, ServerLevel serverLevel) {
    return revive(uuid, serverLevel, null);
  }

  public static boolean revive(UUID uuid, ServerLevel serverLevel, Vec3 position) {
    Optional<SavedNPCEntityEntry> entry = getSpawnableEntry(uuid, getReviveRejection(uuid));
    if (entry.isEmpty()) {
      return false;
    }

    CompoundTag npcData = entry.get().npcData().copy();
    if (position != null) {
      putPosition(npcData, position);
    }
    // LivingEntity#readAdditionalSaveData loads Attributes before Health, setHealth clamps to max.
    npcData.putFloat("Health", Float.MAX_VALUE);
    npcData.remove("DeathTime");
    npcData.remove("HurtTime");
    npcData.remove("Fire");
    return PresetHandler.importPreset(serverLevel, npcData, entry.get().metadata().ownerUUID());
  }

  public static boolean isLoaded(UUID uuid) {
    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(uuid);
    return easyNPC != null && !easyNPC.getEntity().isRemoved();
  }

  public static Optional<String> getSpawnRejection(UUID uuid, boolean force) {
    return getRejection(
        uuid, removalReason -> force || removalReason.isRestorableReason(), "use force to spawn it");
  }

  public static Optional<String> getReviveRejection(UUID uuid) {
    return getRejection(
        uuid,
        removalReason ->
            removalReason == NPCRemovalReason.KILLED || removalReason.isRestorableReason(),
        "use spawn with force instead");
  }

  private static Optional<String> getRejection(
      UUID uuid, Predicate<NPCRemovalReason> isAcceptedReason, String unacceptedReasonHint) {
    if (NPCEntityManager.isNPCDeleted(uuid)) {
      return Optional.of("it was deleted");
    }

    Optional<NPCEntityMetadata> metadata = NPCEntityManager.getMetadata(uuid);
    if (metadata.isEmpty()) {
      return Optional.of("no saved data found");
    }

    NPCRemovalReason removalReason = metadata.get().removalReason();
    if (removalReason == NPCRemovalReason.ON_SHOULDER) {
      return Optional.of("it sits on the shoulder of a player");
    }

    if (isLoaded(uuid)) {
      return Optional.of("it is already loaded");
    }

    if (!isAcceptedReason.test(removalReason)) {
      return Optional.of(
          "it was removed with reason " + removalReason + ", " + unacceptedReasonHint);
    }

    return Optional.empty();
  }

  private static void putPosition(CompoundTag npcData, Vec3 position) {
    ListTag positionTag = new ListTag();
    positionTag.add(DoubleTag.valueOf(position.x));
    positionTag.add(DoubleTag.valueOf(position.y));
    positionTag.add(DoubleTag.valueOf(position.z));
    npcData.put("Pos", positionTag);
  }

  private static Optional<SavedNPCEntityEntry> getSpawnableEntry(
      UUID uuid, Optional<String> spawnRejection) {
    if (spawnRejection.isPresent()) {
      log.warn("Cannot spawn NPC {}: {}", uuid, spawnRejection.get());
      return Optional.empty();
    }

    Optional<SavedNPCEntityEntry> entry = NPCEntityManager.getNPC(uuid);
    if (entry.isEmpty()) {
      log.error("Cannot spawn NPC {}: metadata exists, but its saved entity data is missing", uuid);
    }
    return entry;
  }
}

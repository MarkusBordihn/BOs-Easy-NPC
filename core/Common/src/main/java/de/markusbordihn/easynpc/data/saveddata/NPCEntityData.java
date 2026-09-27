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

package de.markusbordihn.easynpc.data.saveddata;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.Dynamic;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.storage.NPCFileStorage;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.datafix.DataFixTypes;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.saveddata.SavedData;
import net.minecraft.world.level.saveddata.SavedDataType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityData extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_NAME = "easy_npc_index";
  private static final String DATA_METADATA_TAG = "Metadata";
  private static final String DATA_METADATA_UUID_TAG = "UUID";
  private static final String DATA_METADATA_DATA_TAG = "Data";
  private static final Codec<NPCEntityData> CODEC =
      Codec.PASSTHROUGH.comapFlatMap(
          dynamic -> {
            try {
              CompoundTag tag = (CompoundTag) dynamic.convert(NbtOps.INSTANCE).getValue();
              return DataResult.success(loadFromNbt(tag));
            } catch (Exception e) {
              return DataResult.error(() -> "Failed to load NPCEntityData: " + e.getMessage());
            }
          },
          data -> new Dynamic<>(NbtOps.INSTANCE, data.saveToNbt()));
  public static final SavedDataType<NPCEntityData> TYPE =
      new SavedDataType<>(
          DATA_NAME, NPCEntityData::new, CODEC, DataFixTypes.SAVED_DATA_STRUCTURE_FEATURE_INDICES);
  private static NPCEntityData instance;
  private final Map<UUID, NPCEntityMetadata> metadata = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByOwner = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByType = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByDimension = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByPreset = new HashMap<>();
  private final Map<Identifier, Set<UUID>> entriesByCustomIdentifier = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByCustomIdentifierNamespace = new HashMap<>();

  private NPCFileStorage npcFileStorage;

  public NPCEntityData() {}

  private static NPCEntityData loadFromNbt(CompoundTag compoundTag) {
    NPCEntityData data = new NPCEntityData();
    ListTag metadataTag = compoundTag.getList(DATA_METADATA_TAG).orElse(new ListTag());
    for (int i = 0; i < metadataTag.size(); i++) {
      CompoundTag entryTag = metadataTag.getCompound(i).orElse(new CompoundTag());
      UUID uuid = CompoundTagUtils.readUUID(entryTag, DATA_METADATA_UUID_TAG);
      NPCEntityMetadata entityMetadata =
          NPCEntityMetadata.fromCompoundTag(
              entryTag.getCompound(DATA_METADATA_DATA_TAG).orElse(new CompoundTag()));
      if (uuid != null && entityMetadata != null) {
        data.metadata.put(uuid, entityMetadata);
        data.updateCachedMaps(uuid, entityMetadata);
      }
    }
    log.info("Loaded metadata for {} NPC entities from index", data.metadata.size());
    return data;
  }

  public static NPCEntityData get(MinecraftServer server) {
    if (server == null) {
      log.error("Cannot get NPCEntityData: MinecraftServer is null");
      throw new IllegalArgumentException("MinecraftServer cannot be null");
    }

    if (server.overworld() == null) {
      log.error("Cannot get NPCEntityData: Overworld is not yet loaded");
      throw new IllegalStateException("Overworld must be loaded before accessing NPCEntityData");
    }

    NPCEntityData data = server.overworld().getDataStorage().computeIfAbsent(TYPE);

    if (data.npcFileStorage == null) {
      data.npcFileStorage = new NPCFileStorage(Constants.WORLD_DIR);
    }

    return data;
  }

  public static void init(MinecraftServer server) {
    instance = get(server);
  }

  public static NPCEntityData get() {
    if (instance == null) {
      throw new IllegalStateException("NPCEntityData not initialized. Call init(server) first.");
    }

    return instance;
  }

  public void putEntry(UUID uuid, SavedNPCEntityEntry entry) {
    if (uuid == null || entry == null) {
      log.warn("Attempted to put null UUID or entry");
      return;
    }

    NPCEntityMetadata oldMetadata = this.metadata.get(uuid);
    if (oldMetadata != null) {
      this.removeCachedMaps(uuid, oldMetadata);
    }

    this.metadata.put(uuid, entry.metadata());
    this.updateCachedMaps(uuid, entry.metadata());
    this.setDirty();

    if (this.npcFileStorage != null && entry.npcData() != null) {
      this.npcFileStorage.markDirty(uuid, entry.npcData());
    }
  }

  public void removeEntry(UUID uuid) {
    if (uuid == null) {
      return;
    }

    NPCEntityMetadata entityMetadata = this.metadata.remove(uuid);
    if (entityMetadata != null) {
      this.removeCachedMaps(uuid, entityMetadata);
      this.setDirty();
    }

    if (this.npcFileStorage != null) {
      this.npcFileStorage.delete(uuid);
    }
  }

  public Optional<SavedNPCEntityEntry> getEntry(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    NPCEntityMetadata entityMetadata = this.metadata.get(uuid);
    if (entityMetadata == null) {
      return Optional.empty();
    }

    if (this.npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Optional.empty();
    }

    Optional<CompoundTag> npcData = this.npcFileStorage.load(uuid);
    if (npcData.isEmpty()) {
      log.warn("NPC file missing for UUID {}, removing from index", uuid);
      this.metadata.remove(uuid);
      this.removeCachedMaps(uuid, entityMetadata);
      this.setDirty();
      return Optional.empty();
    }

    return Optional.of(new SavedNPCEntityEntry(uuid, npcData.get(), entityMetadata));
  }

  public Collection<SavedNPCEntityEntry> getAllEntries() {
    if (this.npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Collections.emptyList();
    }

    return List.copyOf(this.metadata.keySet()).stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public int getCount() {
    return this.metadata.size();
  }

  public Set<UUID> getAllUUIDs() {
    return Set.copyOf(this.metadata.keySet());
  }

  public boolean hasEntry(UUID uuid) {
    return uuid != null && this.metadata.containsKey(uuid);
  }

  public void evictFromCache(UUID uuid) {
    if (this.npcFileStorage != null && uuid != null) {
      this.npcFileStorage.evictFromCache(uuid);
    }
  }

  public boolean hasCompleteEntry(UUID uuid) {
    NPCEntityMetadata entityMetadata = uuid != null ? this.metadata.get(uuid) : null;
    if (entityMetadata == null) {
      return false;
    }

    if (this.npcFileStorage == null || this.npcFileStorage.exists(uuid)) {
      return true;
    }

    log.warn("NPC file missing for UUID {}, removing from index", uuid);
    this.metadata.remove(uuid);
    this.removeCachedMaps(uuid, entityMetadata);
    this.setDirty();
    return false;
  }

  public Optional<NPCEntityMetadata> getMetadata(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    return Optional.ofNullable(this.metadata.get(uuid));
  }

  public Set<String> getAllEntityTypes() {
    return Set.copyOf(this.entriesByType.keySet());
  }

  public Set<String> getAllDimensions() {
    return Set.copyOf(this.entriesByDimension.keySet());
  }

  public Set<Identifier> getAllCustomIdentifiers() {
    return Set.copyOf(this.entriesByCustomIdentifier.keySet());
  }

  public Set<String> getAllCustomIdentifierNamespaces() {
    return Set.copyOf(this.entriesByCustomIdentifierNamespace.keySet());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByOwner(UUID ownerUUID) {
    return ownerUUID != null
        ? this.resolveEntries(this.entriesByOwner.get(ownerUUID))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByType(String type) {
    return type != null
        ? this.resolveEntries(this.entriesByType.get(type))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByDimension(String dimension) {
    return dimension != null
        ? this.resolveEntries(this.entriesByDimension.get(dimension))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByPreset(UUID presetUUID) {
    return presetUUID != null
        ? this.resolveEntries(this.entriesByPreset.get(presetUUID))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifier(Identifier customIdentifier) {
    return customIdentifier != null
        ? this.resolveEntries(this.entriesByCustomIdentifier.get(customIdentifier))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifierNamespace(String namespace) {
    return namespace != null
        ? this.resolveEntries(this.entriesByCustomIdentifierNamespace.get(namespace))
        : Collections.emptyList();
  }

  private Collection<SavedNPCEntityEntry> resolveEntries(Set<UUID> uuids) {
    if (uuids == null || uuids.isEmpty()) {
      return Collections.emptyList();
    }

    return List.copyOf(uuids).stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  private <K> void removeFromIndex(Map<K, Set<UUID>> map, K key, UUID uuid) {
    if (key == null) {
      return;
    }

    Set<UUID> set = map.get(key);
    if (set != null) {
      set.remove(uuid);
      if (set.isEmpty()) {
        map.remove(key);
      }
    }
  }

  private void updateCachedMaps(UUID entityUUID, NPCEntityMetadata entityMetadata) {
    if (entityMetadata == null) {
      return;
    }

    if (entityMetadata.hasOwner()) {
      this.entriesByOwner
          .computeIfAbsent(entityMetadata.ownerUUID(), ownerUUID -> new HashSet<>())
          .add(entityUUID);
    }

    if (entityMetadata.hasEntityType()) {
      this.entriesByType
          .computeIfAbsent(entityMetadata.entityType(), entityType -> new HashSet<>())
          .add(entityUUID);
    }

    if (entityMetadata.hasDimension()) {
      this.entriesByDimension
          .computeIfAbsent(entityMetadata.dimension(), dimension -> new HashSet<>())
          .add(entityUUID);
    }

    if (entityMetadata.hasPreset()) {
      this.entriesByPreset
          .computeIfAbsent(entityMetadata.presetUUID(), presetUUID -> new HashSet<>())
          .add(entityUUID);
    }

    if (entityMetadata.hasCustomIdentifier()) {
      Identifier customIdentifier = entityMetadata.customIdentifier();
      this.entriesByCustomIdentifier
          .computeIfAbsent(customIdentifier, identifier -> new HashSet<>())
          .add(entityUUID);
      this.entriesByCustomIdentifierNamespace
          .computeIfAbsent(customIdentifier.getNamespace(), namespace -> new HashSet<>())
          .add(entityUUID);
    }
  }

  private void removeCachedMaps(UUID entityUUID, NPCEntityMetadata entityMetadata) {
    if (entityMetadata == null) {
      return;
    }

    this.removeFromIndex(this.entriesByOwner, entityMetadata.ownerUUID(), entityUUID);
    this.removeFromIndex(this.entriesByType, entityMetadata.entityType(), entityUUID);
    this.removeFromIndex(this.entriesByDimension, entityMetadata.dimension(), entityUUID);
    this.removeFromIndex(this.entriesByPreset, entityMetadata.presetUUID(), entityUUID);
    if (entityMetadata.hasCustomIdentifier()) {
      this.removeFromIndex(
          this.entriesByCustomIdentifier, entityMetadata.customIdentifier(), entityUUID);
      this.removeFromIndex(
          this.entriesByCustomIdentifierNamespace,
          entityMetadata.customIdentifier().getNamespace(),
          entityUUID);
    }
  }

  public <E extends Mob> void updateDimension(EasyNPC<E> easyNPC, ServerLevel serverLevel) {
    UUID uuid = easyNPC.getEntityUUID();
    String newDimension = serverLevel.dimension().identifier().toString();
    NPCEntityMetadata old = this.metadata.get(uuid);
    if (old == null || Objects.equals(old.dimension(), newDimension)) {
      return;
    }

    this.removeFromIndex(this.entriesByDimension, old.dimension(), uuid);
    this.metadata.put(uuid, old.withDimension(newDimension));
    this.entriesByDimension.computeIfAbsent(newDimension, dimension -> new HashSet<>()).add(uuid);
    this.setDirty();
  }

  public <E extends Mob> void updateOwner(EasyNPC<E> easyNPC, LivingEntity owner) {
    UUID uuid = easyNPC.getEntityUUID();
    UUID newOwnerUUID = owner != null ? owner.getUUID() : null;
    NPCEntityMetadata old = this.metadata.get(uuid);
    if (old == null || Objects.equals(old.ownerUUID(), newOwnerUUID)) {
      return;
    }

    this.removeFromIndex(this.entriesByOwner, old.ownerUUID(), uuid);
    this.metadata.put(uuid, old.withOwnerUUID(newOwnerUUID));
    if (newOwnerUUID != null) {
      this.entriesByOwner.computeIfAbsent(newOwnerUUID, ownerUUID -> new HashSet<>()).add(uuid);
    }
    this.setDirty();
  }

  public void updateRemovalReason(UUID uuid, NPCRemovalReason reason) {
    NPCEntityMetadata old = this.metadata.get(uuid);
    if (old == null || old.removalReason() == reason) {
      return;
    }

    this.metadata.put(uuid, old.withRemovalReason(reason));
    this.setDirty();
  }

  public int saveAllDirtyNPCs() {
    if (this.npcFileStorage != null) {
      return this.npcFileStorage.saveAllDirty();
    }

    return 0;
  }

  private CompoundTag saveToNbt() {
    CompoundTag compoundTag = new CompoundTag();
    ListTag metadataTag = new ListTag();
    for (Map.Entry<UUID, NPCEntityMetadata> entry : this.metadata.entrySet()) {
      CompoundTag entryTag = new CompoundTag();
      CompoundTagUtils.writeUUID(entryTag, DATA_METADATA_UUID_TAG, entry.getKey());
      entryTag.put(DATA_METADATA_DATA_TAG, entry.getValue().toCompoundTag());
      metadataTag.add(entryTag);
    }
    compoundTag.put(DATA_METADATA_TAG, metadataTag);

    int savedFiles = this.saveAllDirtyNPCs();
    log.debug(
        "Saved metadata for {} NPC entities to index and {} dirty NPC files",
        this.metadata.size(),
        savedFiles);

    return compoundTag;
  }
}

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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.storage.NPCFileStorage;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.saveddata.SavedData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityData extends SavedData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_NAME = "easy_npc_index";
  private static final String DATA_METADATA_TAG = "Metadata";
  private static final String DATA_METADATA_UUID_TAG = "UUID";
  private static final String DATA_METADATA_DATA_TAG = "Data";
  private static NPCEntityData instance;
  private final Map<UUID, NPCEntityMetadata> metadata = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByOwner = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByType = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByDimension = new HashMap<>();
  private final Map<UUID, Set<UUID>> entriesByPreset = new HashMap<>();
  private final Map<ResourceLocation, Set<UUID>> entriesByCustomIdentifier = new HashMap<>();
  private final Map<String, Set<UUID>> entriesByCustomIdentifierNamespace = new HashMap<>();

  private NPCFileStorage npcFileStorage;

  public NPCEntityData() {}

  public static NPCEntityData load(CompoundTag compoundTag, HolderLookup.Provider provider) {
    NPCEntityData data = new NPCEntityData();
    ListTag metadataTag = compoundTag.getList(DATA_METADATA_TAG, Tag.TAG_COMPOUND);
    for (int i = 0; i < metadataTag.size(); i++) {
      CompoundTag entryTag = metadataTag.getCompound(i);
      UUID uuid = entryTag.getUUID(DATA_METADATA_UUID_TAG);
      NPCEntityMetadata meta =
          NPCEntityMetadata.fromCompoundTag(entryTag.getCompound(DATA_METADATA_DATA_TAG));
      if (uuid != null && meta != null) {
        data.metadata.put(uuid, meta);
        data.updateCachedMaps(uuid, meta);
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
    NPCEntityData data =
        server
            .overworld()
            .getDataStorage()
            .computeIfAbsent(
                new SavedData.Factory<>(NPCEntityData::new, NPCEntityData::load, null), DATA_NAME);

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
      removeCachedMaps(uuid, oldMetadata);
    }

    this.metadata.put(uuid, entry.metadata());
    updateCachedMaps(uuid, entry.metadata());
    setDirty();

    if (npcFileStorage != null && entry.npcData() != null) {
      npcFileStorage.markDirty(uuid, entry.npcData());
    }
  }

  public void removeEntry(UUID uuid) {
    if (uuid == null) {
      return;
    }

    NPCEntityMetadata meta = this.metadata.remove(uuid);
    if (meta != null) {
      removeCachedMaps(uuid, meta);
      setDirty();
    }

    if (npcFileStorage != null) {
      npcFileStorage.delete(uuid);
    }
  }

  public Optional<SavedNPCEntityEntry> getEntry(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }

    NPCEntityMetadata meta = this.metadata.get(uuid);
    if (meta == null) {
      return Optional.empty();
    }

    if (npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Optional.empty();
    }

    Optional<CompoundTag> npcData = npcFileStorage.load(uuid);
    if (npcData.isEmpty()) {
      log.warn("NPC file missing for UUID {}, removing from index", uuid);
      this.metadata.remove(uuid);
      removeCachedMaps(uuid, meta);
      setDirty();
      return Optional.empty();
    }

    return Optional.of(new SavedNPCEntityEntry(uuid, npcData.get(), meta));
  }

  public Collection<SavedNPCEntityEntry> getAllEntries() {
    if (npcFileStorage == null) {
      log.error("NPCFileStorage not initialized");
      return Collections.emptyList();
    }

    return this.metadata.keySet().stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  public int getCount() {
    return this.metadata.size();
  }

  public Set<UUID> getAllUUIDs() {
    return Collections.unmodifiableSet(metadata.keySet());
  }

  public boolean hasEntry(UUID uuid) {
    return uuid != null && metadata.containsKey(uuid);
  }

  public Optional<NPCEntityMetadata> getMetadata(UUID uuid) {
    if (uuid == null) {
      return Optional.empty();
    }
    return Optional.ofNullable(metadata.get(uuid));
  }

  public Set<String> getAllEntityTypes() {
    return Collections.unmodifiableSet(entriesByType.keySet());
  }

  public Set<String> getAllDimensions() {
    return Collections.unmodifiableSet(entriesByDimension.keySet());
  }

  public Set<ResourceLocation> getAllCustomIdentifiers() {
    return Collections.unmodifiableSet(entriesByCustomIdentifier.keySet());
  }

  public Set<String> getAllCustomIdentifierNamespaces() {
    return Collections.unmodifiableSet(entriesByCustomIdentifierNamespace.keySet());
  }

  public Collection<SavedNPCEntityEntry> getEntriesByOwner(UUID ownerUUID) {
    return ownerUUID != null
        ? resolveEntries(entriesByOwner.get(ownerUUID))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByType(String type) {
    return type != null ? resolveEntries(entriesByType.get(type)) : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByDimension(String dimension) {
    return dimension != null
        ? resolveEntries(entriesByDimension.get(dimension))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByPreset(UUID presetUUID) {
    return presetUUID != null
        ? resolveEntries(entriesByPreset.get(presetUUID))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifier(
      ResourceLocation customIdentifier) {
    return customIdentifier != null
        ? resolveEntries(entriesByCustomIdentifier.get(customIdentifier))
        : Collections.emptyList();
  }

  public Collection<SavedNPCEntityEntry> getEntriesByCustomIdentifierNamespace(String namespace) {
    return namespace != null
        ? resolveEntries(entriesByCustomIdentifierNamespace.get(namespace))
        : Collections.emptyList();
  }

  private Collection<SavedNPCEntityEntry> resolveEntries(Set<UUID> uuids) {
    if (uuids == null || uuids.isEmpty()) return Collections.emptyList();
    return uuids.stream()
        .map(this::getEntry)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
  }

  private <K> void removeFromIndex(Map<K, Set<UUID>> map, K key, UUID uuid) {
    if (key == null) return;
    Set<UUID> set = map.get(key);
    if (set != null) {
      set.remove(uuid);
      if (set.isEmpty()) map.remove(key);
    }
  }

  private void updateCachedMaps(UUID entityUUID, NPCEntityMetadata meta) {
    if (meta == null) {
      return;
    }

    if (meta.hasOwner()) {
      entriesByOwner.computeIfAbsent(meta.ownerUUID(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasEntityType()) {
      entriesByType.computeIfAbsent(meta.entityType(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasDimension()) {
      entriesByDimension.computeIfAbsent(meta.dimension(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasPreset()) {
      entriesByPreset.computeIfAbsent(meta.presetUUID(), k -> new HashSet<>()).add(entityUUID);
    }

    if (meta.hasCustomIdentifier()) {
      ResourceLocation customIdentifier = meta.customIdentifier();
      entriesByCustomIdentifier
          .computeIfAbsent(customIdentifier, k -> new HashSet<>())
          .add(entityUUID);
      entriesByCustomIdentifierNamespace
          .computeIfAbsent(customIdentifier.getNamespace(), k -> new HashSet<>())
          .add(entityUUID);
    }
  }

  private void removeCachedMaps(UUID entityUUID, NPCEntityMetadata meta) {
    if (meta == null) return;
    removeFromIndex(entriesByOwner, meta.ownerUUID(), entityUUID);
    removeFromIndex(entriesByType, meta.entityType(), entityUUID);
    removeFromIndex(entriesByDimension, meta.dimension(), entityUUID);
    removeFromIndex(entriesByPreset, meta.presetUUID(), entityUUID);
    if (meta.hasCustomIdentifier()) {
      removeFromIndex(entriesByCustomIdentifier, meta.customIdentifier(), entityUUID);
      removeFromIndex(
          entriesByCustomIdentifierNamespace, meta.customIdentifier().getNamespace(), entityUUID);
    }
  }

  public <E extends Mob> void updateDimension(EasyNPC<E> easyNPC, ServerLevel serverLevel) {
    UUID uuid = easyNPC.getEntityUUID();
    String newDimension = serverLevel.dimension().location().toString();
    NPCEntityMetadata old = metadata.get(uuid);
    if (old == null || Objects.equals(old.dimension(), newDimension)) return;
    removeFromIndex(entriesByDimension, old.dimension(), uuid);
    metadata.put(
        uuid,
        new NPCEntityMetadata(
            old.ownerUUID(),
            old.entityType(),
            newDimension,
            old.presetUUID(),
            old.customIdentifier(),
            old.removalReason()));
    entriesByDimension.computeIfAbsent(newDimension, k -> new HashSet<>()).add(uuid);
    setDirty();
  }

  public <E extends Mob> void updateOwner(EasyNPC<E> easyNPC, LivingEntity owner) {
    UUID uuid = easyNPC.getEntityUUID();
    UUID newOwnerUUID = owner != null ? owner.getUUID() : null;
    NPCEntityMetadata old = metadata.get(uuid);
    if (old == null || Objects.equals(old.ownerUUID(), newOwnerUUID)) return;
    removeFromIndex(entriesByOwner, old.ownerUUID(), uuid);
    metadata.put(
        uuid,
        new NPCEntityMetadata(
            newOwnerUUID,
            old.entityType(),
            old.dimension(),
            old.presetUUID(),
            old.customIdentifier(),
            old.removalReason()));
    if (newOwnerUUID != null) {
      entriesByOwner.computeIfAbsent(newOwnerUUID, k -> new HashSet<>()).add(uuid);
    }
    setDirty();
  }

  public void updateRemovalReason(UUID uuid, NPCRemovalReason reason) {
    NPCEntityMetadata old = metadata.get(uuid);
    if (old == null) return;
    metadata.put(
        uuid,
        new NPCEntityMetadata(
            old.ownerUUID(),
            old.entityType(),
            old.dimension(),
            old.presetUUID(),
            old.customIdentifier(),
            reason));
    setDirty();
  }

  public int saveAllDirtyNPCs() {
    if (npcFileStorage != null) {
      return npcFileStorage.saveAllDirty();
    }
    return 0;
  }

  @Override
  public CompoundTag save(CompoundTag compoundTag, HolderLookup.Provider provider) {
    ListTag metadataTag = new ListTag();
    for (Map.Entry<UUID, NPCEntityMetadata> entry : this.metadata.entrySet()) {
      CompoundTag entryTag = new CompoundTag();
      entryTag.putUUID(DATA_METADATA_UUID_TAG, entry.getKey());
      entryTag.put(DATA_METADATA_DATA_TAG, entry.getValue().toCompoundTag());
      metadataTag.add(entryTag);
    }
    compoundTag.put(DATA_METADATA_TAG, metadataTag);

    int savedFiles = saveAllDirtyNPCs();
    if (savedFiles > 0) {
      log.debug(
          "Saved metadata for {} NPC entities to index and {} dirty NPC files",
          this.metadata.size(),
          savedFiles);
    } else {
      log.debug(
          "Saved metadata for {} NPC entities to index (no dirty files)", this.metadata.size());
    }

    return compoundTag;
  }
}
